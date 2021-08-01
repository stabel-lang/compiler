const compiler = require('./compiler.wrapper');

test('Enum type', async () => {
    const wat = await compiler.toWat('main', `
       defstruct: True 
       defstruct: False

       def: main
       : True
    `);

    const result = await compiler.run(wat, 'main');

    // types are sorted alphabetically, so False will get id 0, and True gets id 1.
    expect(result.typeIdForPointer()).toBe(1);
});

test('Struct with annotations', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: True

        def: main
        : True as-int

        def: as-int
        type: True -- Int
        : drop 1
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Compound type', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Person
        : age Int

        def: inc-age
        : age> 1 + >Person

        def: main
        : 1 >Person 19 >age inc-age age>
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(20);
});
