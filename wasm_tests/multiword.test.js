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
