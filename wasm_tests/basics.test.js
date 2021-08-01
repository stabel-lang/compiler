const compiler = require('./compiler.wrapper');

test('Simple expression', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 1 1 +
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(2);
});

test('Function calls', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 1 inc inc dec 2 =

        def: inc
        : 1 +

        def: dec
        : 1 -
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});
