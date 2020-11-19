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

test('Basic quotation', async () => {
    const wat = await compiler.toWat(`
        deftype: Coordinate
        : x Int
        : y Int

        def: update-x
        type: Coordinate [ Int -- Int ] -- Coordinate
        : swap
          dup x>
          -rotate
          !
          >x

        def: main
        entry: true
        : 1 2 >Coordinate
          [ 1 + ] update-x
          x>
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(2);
});
