const Compiler = require('./compiler.js');
const wabt = require('wabt')();

test('todo', async () => {
    const wat = await compileToWat("def: main entry: true : 1 1 +");
    const result = await runCode(wat);

    expect(result).toBe(2);
});

function compileToWat(sourceCode) {
    return new Promise((resolve, reject) => {
        const compiler = Compiler.Elm.Main.init({});

        compiler.ports.compileFinished.subscribe(([ok, output]) => {
            if (ok) {
                resolve(output);
            } else {
                reject(output);
            }
        });

        compiler.ports.compileString.send(sourceCode);
    });
}

async function runCode(wat) {
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
    program.instance.exports.main();

    const memoryView = new Uint32Array(memory.buffer, 0, 20);
    return memoryView[1];
}
