#!/usr/bin/env node

const path = require("path");
const fs = require("fs");

const subCmd = process.argv[2];
const subCmdFlags = process.argv.slice(3);

switch (subCmd) {
    case "compile": compileProject(); break;
    default: printHelp(); break;
}

function compileProject() {
    const cwd = process.cwd();
    const compiler = require(__dirname + "/compiler").Elm.CLI.init({
        flags: cwd
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
                console.log("Compiled successfully");
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
* help: print this help message.
    `);
}
