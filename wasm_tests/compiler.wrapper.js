const Compiler = require('./compiler.js');
const wabtInit = require('wabt');
const stripIndent = require('strip-indent');

exports.toWat = function toWat(entry, sourceCode) {
    return new Promise((resolve, reject) => {
        const compiler = Compiler.Elm.TestCompiler.init({});

        compiler.ports.compileFinished.subscribe(([ok, output]) => {
            if (ok) {
                resolve(output);
            } else {
                reject(output);
            }
        });

        compiler.ports.compileString.send([entry, stripIndent(sourceCode)]);
    });
}

exports.run = async function run(wat, functionName) {
    const wabt = await wabtInit();
    const wasmModule = wabt.parseWat('tmp', wat).toBinary({}).buffer;

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

    typeIdForPointer(index) {
        const pointer = this.stackElement(index || 0);
        const wordPointer = pointer / 4;
        return this.memoryView[wordPointer];
    }
}
