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

