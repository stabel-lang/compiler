#!/usr/bin/env node

const path = require("path");
const fs = require("fs");
const wabtInit = require("wabt");

const stdLibPath = path.resolve(__dirname, "..", "stdlib");
const subCmd = process.argv[2];
const subCmdFlags = process.argv.slice(3);

switch (subCmd) {
    case "compile": compileProject(); break;
    case "run": runProject(); break;
    default: printHelp(); break;
}

function runProject() {
    if (subCmdFlags.length !== 1) {
        console.error("run command requires exactly one argument: the function to run.");
        return;
    }

    compileProject(subCmdFlags[0]);
}

function compileProject(entryPoint) {
    const cwd = process.cwd();
    const rootJsonMetaPath = path.join(cwd, "play.json");
    if (!fs.existsSync(rootJsonMetaPath)) {
        console.error("No 'play.json' file found in this directory.");
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
                    .filter(name => name.endsWith(".play"))
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
            case "compilationDone":
                if (typeof entryPoint === "undefined") {
                    console.log("Compiled successfully");
                } else {
                    executeWat(msg.wast, entryPoint);
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
        .filter(dir => fs.readdirSync(dir).indexOf('play.json') >= 0);
}

function printHelp() {
    console.log(`
Play compiler. Alpha-2.

Possible options are:
* compile: compile the project.
* run <function_name>: compile the project, and execute the given function.
* help: print this help message.
    `);
}

async function executeWat(wat, entryPointName) {
    const wabt = await wabtInit();
    const wasmModule = wabt.parseWat('tmp', wat).toBinary({}).buffer;

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
    const returnValue = memView[3].toString(); 
    console.log(returnValue);
}
