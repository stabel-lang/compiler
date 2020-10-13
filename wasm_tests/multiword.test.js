const compiler = require('./compiler.wrapper');

test('Simple case', async () => {
    const wat = await compiler.toWat(`
        defunion: Bool
        : True
        : False

        deftype: True
        deftype: False

        defmulti: to-int
        when: True
          drop 100
        when: False
          drop 75

        def: main
        entry: true
        : >True to-int >False to-int -
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(25);
});

// Same as above, but without an explicit False branch.
test('Default branch', async () => {
    const wat = await compiler.toWat(`
        defunion: Bool
        : True 
        : False

        deftype: True
        deftype: False

        defmulti: to-int
        when: True
          drop 100
        : drop 75

        def: main
        entry: true
        : >True to-int 
          >False to-int 
          -
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(25);
});

test('Multiple arguments', async () => {
    const wat = await compiler.toWat(`
        defunion: Beings
        : Person 
        : Dog

        deftype: Person
        : age Int

        deftype: Dog
        : man-years Int

        defmulti: add-to-age
        when: Person
          swap dup age>
          -rotate +
          >age
        when: Dog
          4 * 
          swap dup man-years>
          -rotate +
          >man-years

        defmulti: get-man-age
        when: Person
          age>
        when: Dog
          man-years>

        def: main
        entry: true
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
    const wat = await compiler.toWat(`
        defunion: List a
        : NonEmptyList a 
        : EmptyList

        deftype: NonEmptyList a
        : first a
        : rest List a

        deftype: EmptyList

        defmulti: first-or-default
        when: NonEmptyList
          drop first>
        when: EmptyList
          swap drop

        def: main
        entry: true
        : 1 >EmptyList >NonEmptyList
          0 first-or-default
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Recursive word', async () => {
    const wat = await compiler.toWat(`
        defunion: List a
        : NonEmptyList a 
        : EmptyList

        deftype: NonEmptyList a
        : first a
        : rest List a

        deftype: EmptyList

        def: sum
        : 0 sum-helper

        defmulti: sum-helper
        type: (List a) Int -- Int
        when: NonEmptyList
          swap dup first> rotate rest> rotate + sum-helper
        when: EmptyList
          swap drop

        def: main
        entry: true
        : 1 2 3 >EmptyList >NonEmptyList >NonEmptyList >NonEmptyList
          sum
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(6);
});

test('Int case', async () => {
    const wat = await compiler.toWat(`
        defunion: Bool
        : Int
        : NoInt

        deftype: NoInt

        defmulti: double
        when: Int
          2 *
        when: NoInt
          drop 0

        def: main
        entry: true
        : 4 double
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(8);
});

test('Int match', async () => {
    const wat = await compiler.toWat(`
        defmulti: double
        when: Int( value 0 )
          drop 2
        when: Int
          2 *

        def: main
        entry: true
        : 0 double
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(2);
});

test('Int match (reverse)', async () => {
    const wat = await compiler.toWat(`
        defmulti: double
        when: Int( value 0 )
          drop 2
        when: Int
          2 *

        def: main
        entry: true
        : 6 double
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(12);
});

test('Correct Int boxing behaviour', async () => {
    const wat = await compiler.toWat(`
        defmulti: add
        when: Int( value 0 )
          swap 
          drop 2
          swap
          +
        when: Int
          +

        def: main
        entry: true
        : 10 6 add
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(16);
});

test('Correct Int boxing behaviour when mismatch between word input size and stack size', async () => {
    const wat = await compiler.toWat(`
        deftype: Nil

        defmulti: inc-zero
        when: Int( value 0 )
          swap 
          drop 1
          swap
        when: Int

        def: main
        entry: true
        : 0 >Nil inc-zero
          drop 
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(1);
});

test('Generic case', async () => {
    const wat = await compiler.toWat(`
        defunion: Maybe a
        : a
        : Nil

        deftype: Nil

        defmulti: map
        type: (Maybe a) [ a -- b ] -- (Maybe b)
        when: a
          !
        when: Nil
          drop

        defmulti: with-default
        type: (Maybe a) a -- a
        when: a
          drop
        when: Nil
          swap drop

        def: main
        entry: true
        : 10
          [ 1 - ] map
          0 with-default
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(9);
});
