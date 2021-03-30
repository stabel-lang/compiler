#!/usr/bin/env node

const subCmd = process.argv[2];
const subCmdFlags = process.argv.slice(3);

switch (subCmd) {
    case "check": checkProject(); break;
    default: printHelp(); break;
}

function checkProject() {
    console.log("TODO: compile project");
}

function printHelp() {
    console.log(`
Play compiler. Alpha-2.

Possible options are:
* check: compile the project.
* help: print this help message.
    `);
}
