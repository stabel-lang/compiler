const compiler = require('./compiler.wrapper');

test('Square', async () => {
    const wat = await compiler.toWat(`
        def: main
        entry: true
        type: -- Int
        : 5 square

        def: square
        type: Int -- Int
        : dup *
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(25);
});

test('Over', async() => {
    const wat = await compiler.toWat(`
        def: main
        entry: true
        type: -- Int
        : 1 2 over - + 2 =

        def: over
        type: a b -- a b a
        : swap dup rotate
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});
