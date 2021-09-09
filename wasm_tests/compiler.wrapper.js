const Compiler = require('./compiler.js');
const wabtInit = require('wabt');
const stripIndent = require('strip-indent');

exports.toWat = function toWat(entry, sourceCode) {
    return new Promise((resolve, reject) => {
        const compiler = Compiler.Elm.TestCompiler.init({
            flags: {
                __type: 'CompileString',
                entryPoint: entry,
                sourceCode: stripIndent(sourceCode)
            }
        });

        compiler.ports.compileFinished.subscribe(([ok, output]) => {
            if (ok) {
                if (output.indexOf('unreachable') >= 0) {
                    reject('output contains \'unreachable\' instruction');
                }

                resolve(output);
            } else {
                reject(output);
            }
        });
    });
}

function toProjectWat(payload) {
    return new Promise((resolve, reject) => {
        const compiler = Compiler.Elm.TestCompiler.init({
            flags: {
                __type: 'CompileProject',
                entryPoint: payload.entryPoint,
                modules: payload.modules.map((mod) => {
                    mod.source = stripIndent(mod.source)
                    return mod
                })
            }
        });

        compiler.ports.compileFinished.subscribe(([ok, output]) => {
            if (ok) {
                if (output.indexOf('unreachable') >= 0) {
                    reject('output contains \'unreachable\' instruction');
                }

                resolve(output);
            } else {
                reject(output);
            }
        });
    });
}

exports.toProjectWat = toProjectWat;

exports.toStringWat = function toStringWat(entry, sourceCode) {
    return toProjectWat({
        entryPoint: `/author/sample/core/${entry}`,
        modules: [
            {
                package: 'stabel/standard_library',
                module: 'string',
                source: `
                    defmodule:
                    exposing: String from-bytes
                    :

                    defstruct: String
                    : content Array Int

                    def: from-bytes
                    type: (Array Int) -- String
                    : >String
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: stripIndent(sourceCode)
            }
        ]
    });
}

exports.run = async function run(wat, functionName) {
    const wabt = await wabtInit();
    const wasmModule = wabt.parseWat('tmp', wat, {
        bulk_memory: true
    }).toBinary({}).buffer;

    const memory = new WebAssembly.Memory({
        initial: 1
    });

    const imports = {
        host: {
            memory: memory
        }
    };

    const program = await WebAssembly.instantiate(wasmModule, imports);
    program.instance.exports[functionName]();

    return new ExecutionResult(memory.buffer);
}

class ExecutionResult {
    constructor(memoryBuffer) {
        this.memoryView = new Uint32Array(memoryBuffer, 0, 512);
    }

    stackElement(index) {
        // The first three I32 positions are used for stack and heap information
        // The fourth position is the first element of the stack
        return this.memoryView[3 + (index || 0)];
    }

    stringElement(index) {
        // String structure => typeId | array 
        const strPointer = this.stackElement(index);
        const strOffset = strPointer / 4;

        const arrayPointer = this.memoryView[strOffset + 1];
        const arrayOffset = arrayPointer / 4;

        const strLen = this.memoryView[arrayOffset];

        const content = [];
        const strValueOffset = arrayOffset + 1;
        for (let i = 0; i < strLen; i++) {
            content.push(this.memoryView[strValueOffset + i]);
        }

        const decoder = new TextDecoder();
        const strAsUtf8 = new Uint8Array(content);
        return decoder.decode(strAsUtf8);
    }

    typeIdForPointer(index) {
        const pointer = this.stackElement(index || 0);
        const wordPointer = pointer / 4;
        return this.memoryView[wordPointer];
    }
}
