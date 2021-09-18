const compiler = require('./compiler.wrapper');

test('Simple case', async () => {
    const wat = await compiler.toWat('main', `
        defunion: Bool
        : True
        : False

        defstruct: True
        defstruct: False

        defmulti: to-int
        : True
          drop 100
        : False
          drop 75

        def: main
        : True> to-int 
          False> to-int 
          -
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(25);
});

// Same as above, but without an explicit False branch.
test('Default branch', async () => {
    const wat = await compiler.toWat('main', `
        defunion: Bool
        : True 
        : False

        defstruct: True
        defstruct: False

        defmulti: to-int
        : True
          drop 100
        else: drop 75

        def: main
        : True> to-int 
          False> to-int 
          -
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(25);
});

test('Multiple arguments', async () => {
    const wat = await compiler.toWat('main', `
        defunion: Beings
        : Person 
        : Dog

        defstruct: Person
        : age Int

        defstruct: Dog
        : man-years Int

        defmulti: add-to-age
        : Person
          swap dup age>
          -rotate +
          >age
        : Dog
          4 * 
          swap dup man-years>
          -rotate +
          >man-years

        defmulti: get-man-age
        : Person
          age>
        : Dog
          man-years>

        def: main
        : 18 >Person 10 add-to-age 
          0 >Dog 2 add-to-age 
          get-man-age swap 
          get-man-age swap 
          -
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(20);
});

test('Generic arguments', async () => {
    const wat = await compiler.toWat('main', `
        defunion: List a
        : NonEmptyList a 
        : EmptyList

        defstruct: NonEmptyList a
        : first a
        : rest List a

        defstruct: EmptyList

        defmulti: first-or-default
        : NonEmptyList
          drop first>
        : EmptyList
          swap drop

        def: main
        : 1 EmptyList> >NonEmptyList
          0 first-or-default
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Recursive word', async () => {
    const wat = await compiler.toWat('main', `
        defunion: List a
        : NonEmptyList a 
        : EmptyList

        defstruct: NonEmptyList a
        : first a
        : rest List a

        defstruct: EmptyList

        def: sum
        : 0 sum-helper

        defmulti: sum-helper
        type: (List a) Int -- Int
        : NonEmptyList
          swap dup first> rotate rest> rotate + sum-helper
        : EmptyList
          swap drop

        def: main
        : 1 2 3 EmptyList> >NonEmptyList >NonEmptyList >NonEmptyList
          sum
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(6);
});

test('Int case', async () => {
    const wat = await compiler.toWat('main', `
        defunion: IntParseResult
        : Int
        : NoInt

        defstruct: NoInt

        defmulti: double
        : Int
          2 *
        : NoInt
          drop 0

        def: main
        : 4 double
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(8);
});

test('Int match', async () => {
    const wat = await compiler.toWat('main', `
        defmulti: double
        : Int( value 0 )
          drop 2
        : Int
          2 *

        def: main
        : 0 double
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(2);
});

test('Int match (reverse)', async () => {
    const wat = await compiler.toWat('main', `
        defmulti: double
        : Int( value 0 )
          drop 2
        : Int
          2 *

        def: main
        : 6 double
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(12);
});

test('Correct Int boxing behaviour', async () => {
    const wat = await compiler.toWat('main', `
        defmulti: add
        : Int( value 0 )
          swap 
          drop 2
          swap
          +
        : Int
          +

        def: main
        : 10 6 add
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(16);
});

test('Correct Int boxing behaviour when mismatch between word input size and stack size', async () => {
    const wat = await compiler.toWat('main', `
        defstruct: Nil

        defmulti: inc-zero
        : Int( value 0 )
          swap 
          drop 1
          swap
        : Int

        def: main
        : 0 Nil> inc-zero
          drop 
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Generic case', async () => {
    const wat = await compiler.toWat('main', `
        defunion: Maybe a
        : a
        : Nil

        defstruct: Nil

        defmulti: map
        type: (Maybe a) [ a -- b ] -- (Maybe b)
        : a
          !
        : Nil
          drop

        defmulti: with-default
        type: (Maybe a) a -- a
        : a
          drop
        : Nil
          swap drop

        def: main
        : 10
          [ 1 - ] map
          0 with-default
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(9);
});

test('Cyclic case', async () => {
    const wat = await compiler.toWat('main', `
        def: main
        : 10 count-down

        defmulti: count-down
        type: Int -- Int
        : Int( value 0 )
          drop 0
        : Int
          dec-count-down

        def: dec-count-down
        : 1 - count-down
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});
