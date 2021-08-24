const compiler = require('./compiler.wrapper');

test('Empty', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : {} array-length
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});

test('Simple literal', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : { 1 2 3 4 5 } array-length
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(5);
});

test('Pushing', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : {}
          2 array-push
          1 array-push
          array-length
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(2);
});

