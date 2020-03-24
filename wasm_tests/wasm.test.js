const Compiler = require('./compiler.js');
const wabt = require('wabt')();

test('Simple expression', async () => {
    const wat = await compileToWat(`
        def: main
        entry: true
        : 1 1 +
    `);
    const result = await runCode(wat, 'main');

    expect(result).toBe(2);
});

test('Function calls', async () => {
    const wat = await compileToWat(`
        def: main
        entry: true
        : 1 inc inc

        def: inc
        : 1 +
    `);
    const result = await runCode(wat, 'main');

    expect(result).toBe(3);
});

// Helpers

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

async function runCode(wat, functionName) {
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

    const memoryView = new Uint32Array(memory.buffer, 0, 20);
    return memoryView[1];
}
