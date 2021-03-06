const compiler = require('./compiler.wrapper');

test('Addition', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 3 3 +
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(6);
});

test('Subtraction', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 10 1 -
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(9);
});

test('Multiplication', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 5 3 *
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(15);
});

test('Division', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 10 5 div
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(2);
});
