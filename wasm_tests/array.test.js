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

test('Get first', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : { 5 6 7 }
          0 array-get
          swap drop
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(5);
});

test('Get second', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : { 5 6 7 }
          1 array-get
          swap drop
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(6);
});

test('Get last', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : { 5 6 7 }
          2 array-get
          swap drop
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(7);
});

test('Get succeess', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : { 5 6 7 }
          1 array-get
          drop
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Get out of lower bound', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : { 5 6 7 }
          0 1 -
          array-get
          drop
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});

test('Set works', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : { 5 6 7 8 }
          20
          2 array-set
          2 array-get
          swap drop
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(20);
});

