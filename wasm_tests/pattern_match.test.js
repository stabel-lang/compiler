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

test('Multiple arguments', async () => {
    const wat = await compiler.toWat(`
        deftype: Point
        : { 
          first: Int
          second: Int
        }

        defmulti: origo?
        when: Point( first 0 second 0 )
          drop 1
        : drop 0

        def: main
        entry: true
        : 0 0 >Point origo?
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Multiple arguments reverse case', async () => {
    const wat = await compiler.toWat(`
        deftype: Point
        : { 
          first: Int
          second: Int
        }

        defmulti: origo?
        when: Point( first 0 second 0 )
          drop 1
        : drop 0

        def: main
        entry: true
        : 0 1 >Point origo?
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});

test('Recursive match', async () => {
    const wat = await compiler.toWat(`
        deftype: Box
        : { value: Int }

        deftype: BoxOfBox
        : { box: Box }

        defmulti: deep-one?
        when: BoxOfBox( box Box( value 1 ) )
          drop 1
        : drop 0

        def: main
        entry: true
        : 1 >Box >BoxOfBox deep-one?
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Recursive match reverse case', async () => {
    const wat = await compiler.toWat(`
        deftype: Box
        : { value: Int }

        deftype: BoxOfBox
        : { box: Box }

        defmulti: deep-one?
        when: BoxOfBox( box Box( value 1 ) )
          drop 1
        : drop 0

        def: main
        entry: true
        : 2 >Box >BoxOfBox deep-one?
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});
