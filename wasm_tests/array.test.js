const compiler = require('./compiler.wrapper');

test('Simple literal', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : { 1 2 3 4 5 } array-length
    `);

    console.log(wat);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(2);
});
