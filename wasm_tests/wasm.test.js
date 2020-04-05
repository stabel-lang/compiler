const Compiler = require('./compiler.js');
const wabt = require('wabt')();

test('Simple expression', async () => {
    const wat = await compileToWat(`
        def: main
        entry: true
        : 1 1 +
    `);

    const result = await runCode(wat, 'main');

    expect(result.valueOnBottomOfStack()).toBe(2);
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

    expect(result.valueOnBottomOfStack()).toBe(3);
});

test('Enum type', async () => {
    const wat = await compileToWat(`
       deftype: True 
       deftype: False

       def: main
       entry: true
       : >True
    `);

    const result = await runCode(wat, 'main');

    // types are sorted alphabetically, so False will get id 0, and True gets id 1.
    expect(result.typeIdForPointer()).toBe(1);
});

test('Compound type', async () => {
    const wat = await compileToWat(`
        deftype: Person
        : { age: Int }

        def: inc-age
        : age> 1 + >Person

        def: main
        entry: true
        : 0 >Person 19 >age inc-age age>
    `);

    const result = await runCode(wat, 'main');

    expect(result.valueOnBottomOfStack()).toBe(20);
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

    return new ExecutionResult(memory.buffer);
}

class ExecutionResult {
    constructor(memoryBuffer) {
        this.memoryView = new Uint32Array(memoryBuffer, 0, 512);
    }

    valueOnBottomOfStack() {
        // The first three I32 positions are used for stack and heap information
        // The fourth position is the first element of the stack
        return this.memoryView[3];
    }

    typeIdForPointer() {
        const pointer = this.valueOnBottomOfStack();
        const wordPointer = pointer / 4;
        return this.memoryView[wordPointer];
    }
}
