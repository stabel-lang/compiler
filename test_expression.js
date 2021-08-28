// Execute with: node --inspect-brk ./test_expression.js
// Then open chrome and go to: chrome://inspect

const compiler = require('./wasm_tests/compiler.wrapper');
const wabtInit = require('wabt');

const memory = new WebAssembly.Memory({
    initial: 1
});

global.memView = new Uint32Array(memory.buffer, 0, 512);

async function init() {
    const wabt = await wabtInit();
    const wat = await compiler.toWat('main', `
        def: main
        : { 2 3 4 }
          0 1 -
          array-get
          drop
    `);

    const wasmModule = wabt.parseWat('tmp', wat, {
        bulk_memory: true
    }).toBinary({}).buffer;

    const imports = {
        host: {
            memory: memory
        }
    };

    const program = await WebAssembly.instantiate(wasmModule, imports);
    program.instance.exports.main();
}

init();
