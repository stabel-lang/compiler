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
        : 1 inc inc

        def: inc
        : 1 +
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(3);
});
