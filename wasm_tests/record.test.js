const compiler = require('./compiler.wrapper');

test('Enum type', async () => {
    const wat = await compiler.toWat(`
       defstruct: True 
       defstruct: False

       def: main
       entry: true
       : >True
    `);

    const result = await compiler.run(wat, 'main');

    // types are sorted alphabetically, so False will get id 0, and True gets id 1.
    expect(result.typeIdForPointer()).toBe(1);
});

test('Compound type', async () => {
    const wat = await compiler.toWat(`
        defstruct: Person
        : age Int

        def: inc-age
        : age> 1 + >Person

        def: main
        entry: true
        : 1 >Person 19 >age inc-age age>
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(20);
});
