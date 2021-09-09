#!/usr/bin/env node

const path = require("path");
const fs = require("fs");
const wabtInit = require("wabt");

const stdLibPath = path.resolve(__dirname, "..", "stdlib");
const cwd = process.cwd();

const subCmd = process.argv[2];
const subCmdFlags = process.argv.slice(3);

switch (subCmd) {
    case "init": initProject(); break;
    case "run": runProject(); break;
    case "compile": compileProject(); break;
    default: printHelp(); break;
}

function initProject() {
    if (subCmdFlags.length !== 1) {
        console.error("'init' requires exactly one argument: the package name.");
        return;
    }

    fs.mkdirSync(path.join(cwd, "src"));

    const stabelJson = {
        name: subCmdFlags[0],
        version: "0.1.0",
        "language-version": "0.2.0",
        "execute": `/${subCmdFlags[0]}/main/execute`,
        "exposed-modules": [
            "main"
        ],
        dependencies: {
            "stabel/standard_library": "0.2.1"
        },
        "package-paths": []
    };

    fs.writeFileSync(path.join(cwd, "stabel.json"), JSON.stringify(stabelJson, null, 4));

    fs.writeFileSync(path.join(cwd, "src", "main.stbl"), `
def: execute
: "hello, world!"
`.trim());
}

function runProject() {
    let entryPoint = null;

    if (subCmdFlags.length > 1) {
        console.error("'run' requires exactly one argument: the function to run.");
        return;
    } else if (subCmdFlags.length !== 1) {
        const stabelConfig = JSON.parse(fs.readFileSync(path.join(cwd, "stabel.json")));
        if (typeof stabelConfig.execute === "undefined") {
            console.error("what function should be run? Nothing found in 'execute' property in stabel.json, nor was any function provided on the command line.");
            return;
        }

        entryPoint = stabelConfig.execute;
    } else {
        entryPoint = subCmdFlags[0];
    }

    compileProject(entryPoint);
}

function compileProject(entryPoint) {
    const rootJsonMetaPath = path.join(cwd, "stabel.json");
    if (!fs.existsSync(rootJsonMetaPath)) {
        console.error("No 'stabel.json' file found in this directory.");
        return;
    }

    const compiler = require(__dirname + "/compiler").Elm.CLI.init({
        flags: {
            projectDir: cwd,
            entryPoint: typeof entryPoint === "undefined" ? null : entryPoint,
            stdLibPath: stdLibPath
        }
    });

    compiler.ports.outgoingPort.subscribe((msg) => {
        switch (msg.type) {
            case 'readFile':
                const filePath = path.join(msg.path, msg.fileName);
                const fileContent = fs.readFileSync(filePath, { encoding: "utf-8" });

                compiler.ports.incomingPort.send({
                    type: "fileContents",
                    path: msg.path,
                    fileName: msg.fileName,
                    content: fileContent
                });
                break;
            case "resolvePackageModules":
                const srcPath = path.join(msg.path, "src");
                const childrenFiles = expandDir(msg.path)("src")
                    .filter(name => name.endsWith(".stbl"))
                    .map(name => name.replace(srcPath + "/", ""));

                compiler.ports.incomingPort.send({
                    type: "resolvedPackageModules",
                    package: msg.package,
                    modules: childrenFiles
                });
                break;
            case "resolveDirectories":
                const subFolders = packageDirs(msg.path);

                compiler.ports.incomingPort.send({
                    type: "resolvedDirectories",
                    parentDir: msg.path,
                    paths: subFolders
                });
                break;
            case "readFilesToReportError":
                const files = msg.paths.reduce((acc, path) => { 
                    acc[path] = fs.readFileSync(path, { encoding: "utf-8" });
                    return acc;
                }, {});

                compiler.ports.incomingPort.send({
                    type: "filesForErrorReporting",
                    files: files
                });
                break;
            case "compilationDone":
                if (typeof entryPoint === "undefined") {
                    console.log("Compiled successfully");
                } else {
                    executeWat(msg.wast, entryPoint, msg.isStringReturned);
                }
                break;
            case "compilationFailure":
                console.error(msg.error);
                break;
            default:
                console.error("Unknown message received from Elm compiler: ", msg);
                break;
        }
    });
}

function expandDir(basePath) {
    return (dirPath) => {
        const srcPath = path.join(basePath, dirPath);

        if (fs.statSync(srcPath).isFile()) {
            return srcPath;
        }

        const children = fs.readdirSync(srcPath);
        return children.flatMap(expandDir(srcPath));
    };
}

function packageDirs(parentDir) {
    return fs.readdirSync(parentDir)
        .map(dir => path.join(parentDir, dir))
        .filter(dir => fs.statSync(dir).isDirectory())
        .filter(dir => fs.readdirSync(dir).indexOf('stabel.json') >= 0);
}

function printHelp() {
    console.log(`
Stabel v0.2.1-alpha

Possible options are:
* init <package_name>: initialize a new project in the current directory.
* compile: compile the project.
* run: compile the project and execute the default function.
* run <function_name>: compile the project and execute the given function.
* help: print this help message.
    `);
}

async function executeWat(wat, entryPointName, isStringReturned) {
    const wabt = await wabtInit();
    const wasmModule = wabt.parseWat('tmp', wat, {
        bulk_memory: true
    }).toBinary({}).buffer;

    const memory = new WebAssembly.Memory({
        initial: 10
    });

    const imports = {
        host: {
            memory: memory
        }
    };

    const program = await WebAssembly.instantiate(wasmModule, imports);
    const entryPoint = program.instance.exports[entryPointName];
    if (typeof entryPoint === 'undefined') {
        console.error("Could not find a function named '" + entryPointName + "'");
        return;
    }

    entryPoint();

    const memView = new Int32Array(memory.buffer);
    // First three i32 elements are stack and heap information
    const returnValue = memView[3]; 

    if (isStringReturned) {
        const returnStr = fetchStringValue(memView, returnValue);
        console.log(returnStr);
    } else {
        console.log(returnValue);
    }
}

function fetchStringValue(memView, strPointer) {
    // String structure => typeId | array 
    const strOffset = strPointer / 4;

    const arrayPointer = memView[strOffset + 1];
    const arrayOffset = arrayPointer / 4;

    const strLen = memView[arrayOffset];

    const content = [];
    const strValueOffset = arrayOffset + 1;
    for (let i = 0; i < strLen; i++) {
        content.push(memView[strValueOffset + i]);
    }

    const decoder = new TextDecoder();
    const strAsUtf8 = new Uint8Array(content);
    return decoder.decode(strAsUtf8);
}
