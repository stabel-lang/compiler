const compiler = require('./compiler.wrapper');

test('Basic quotation', async () => {
    const wat = await compiler.toWat(`
        def: main
        entry: true
        : 2 [ inc dec dec ] !

        def: inc
        : 1 +

        def: dec
        : 1 -
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});
