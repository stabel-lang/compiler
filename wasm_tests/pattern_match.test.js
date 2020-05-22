const compiler = require('./compiler.wrapper');

test('Basic pattern match', async () => {
    const wat = await compiler.toWat(`
        deftype: Box
        : { value: Int }

        defmulti: not
        when: Box( value 0 )
          drop 1
        when: Box
          drop 0

        def: main
        entry: true
        : 0 >Box not
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Basic pattern match with default implementation', async () => {
    const wat = await compiler.toWat(`
        deftype: Box
        : { value: Int }

        defmulti: not
        when: Box( value 0 )
          drop 1
        : drop 0

        def: main
        entry: true
        : 0 >Box not
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Basic pattern match reverse case', async () => {
    const wat = await compiler.toWat(`
        deftype: Box
        : { value: Int }

        defmulti: not
        when: Box( value 0 )
          drop 1
        : drop 0

        def: main
        entry: true
        : 1 >Box not
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});
