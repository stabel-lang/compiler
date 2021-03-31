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
        console.log(msg);
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
            default:
                console.error("Unknown message received from Elm compiler: ", msg);
                break;
        }
    });
}

function printHelp() {
    console.log(`
Play compiler. Alpha-2.

Possible options are:
* compile: compile the project.
* help: print this help message.
    `);
}
