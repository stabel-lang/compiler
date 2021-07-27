const compiler = require('./compiler.wrapper');

test('Basic pattern match', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Box
        : value Int

        defmulti: not
        : Box( value 0 )
          drop 1
        : Box
          drop 0

        def: main
        : 0 >Box not
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Basic pattern match with default implementation', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Box
        : value Int

        defmulti: not
        : Box( value 0 )
          drop 1
        else: drop 0

        def: main
        : 0 >Box not
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Basic pattern match reverse case', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Box
        : value Int

        defmulti: not
        : Box( value 0 )
          drop 1
        else: drop 0

        def: main
        : 1 >Box not
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});

test('Multiple arguments', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Point
        : first Int
        : second Int

        defmulti: origo?
        : Point( first 0 second 0 )
          drop 1
        else: drop 0

        def: main
        : 0 0 >Point origo?
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Multiple arguments reverse case', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Point
        : first Int
        : second Int

        defmulti: origo?
        : Point( first 0 second 0 )
          drop 1
        else: drop 0

        def: main
        : 0 1 >Point origo?
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});

test('Recursive match', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Box
        : value Int

        defstruct: BoxOfBox
        : box Box

        defmulti: deep-one?
        : BoxOfBox( box Box( value 1 ) )
          drop 1
        else: drop 0

        def: main
        : 1 >Box >BoxOfBox deep-one?
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Recursive match reverse case', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Box
        : value Int

        defstruct: BoxOfBox
        : box Box

        defmulti: deep-one?
        : BoxOfBox( box Box( value 1 ) )
          drop 1
        else: drop 0

        def: main
        : 2 >Box >BoxOfBox deep-one?
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});

test('Generic case', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Nothing

        defmulti: with-default
        : a
          drop
        : Nothing
          swap drop

        def: main
        : 5
          10 with-default

          Nothing
          10 with-default

          +
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(15);
});

test('Generic case with type annotations', async () => {
    const wat = await compiler.toWat('main', `
        defunion: Maybe a
        : a
        : Nothing

        defstruct: Nothing

        defmulti: with-default
        type: (Maybe a) a -- a
        : a
          drop
        : Nothing
          swap drop

        def: main
        : 5
          10 with-default

          Nothing
          10 with-default

          +
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(15);
});

test('Generic case with type annotations and else keyword', async () => {
    const wat = await compiler.toWat('main', `
        defunion: Maybe a
        : a
        : Nothing

        defstruct: Nothing

        defmulti: with-default
        type: (Maybe a) a -- a
        : a
          drop
        else: 
          swap drop

        def: main
        : 5
          15 with-default

          Nothing
          2 with-default

          +
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(7);
});

test('Unions as struct member', async () => {
    const wat = await compiler.toWat('main', `
        defunion: Bool
        : True
        : False

        defstruct: True
        defstruct: False

        defstruct: Box
        : value Bool

        defmulti: true?
        type: Box -- Bool
        : Box( value True )
          value>
        else: drop False

        defmulti: as-int
        type: Bool -- Int
        : True
          drop 20
        : False
          drop 10

        def: main
        : True >Box
          true? as-int
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(20);
});
