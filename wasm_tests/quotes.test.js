const compiler = require('./compiler.wrapper');

test('Basic quotation', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 2 [ inc dec dec ] !

        def: inc
        : 1 +

        def: dec
        : 1 -
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Basic quotation with type signature', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 1 
          [ 1 + ] apply-to-num
          [ 1 - ] apply-to-num

        def: apply-to-num
        type: Int [ Int -- Int ] -- Int
        : !
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Can reference function inline before it\'s defined', async () => {
    const wat = await compiler.toWat('main', `
        def: a
        : 1 [ inc ] !

        def: inc
        : 1 +
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(2);
});

test('Update function', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Coordinate
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
        : 1 2 >Coordinate
          [ 1 + ] update-x
          x>
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(2);
});

test('Inline function within inline function', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 1
          [ 1 [ 1 + ] ! + ]
          !
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(3);
});
