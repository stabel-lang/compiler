const compiler = require('./wasm_tests/compiler.wrapper');
const wabt = require('wabt')();

const memory = new WebAssembly.Memory({
    initial: 1
});

global.memView = new Uint32Array(memory.buffer, 0, 512);

async function init() {
    const wat = await compiler.toWat(`
        deftype: Box
        : { value: Int }

        deftype: BoxOfBox
        : { box: Box }

        defmulti: deep-one?
        when: BoxOfBox( box Box( value 1 ) )
          drop 1
        : drop 0

        def: main
        entry: true
        : 1 >Box >BoxOfBox deep-one?
    `);

    const wasmModule = wabt.parseWat('tmp', wat).toBinary({}).buffer;

    const imports = {
        host: {
            memory: memory
        }
    };

    const program = await WebAssembly.instantiate(wasmModule, imports);
    debugger;
    program.instance.exports.main();
}

init();
