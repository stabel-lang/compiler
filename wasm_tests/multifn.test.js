const compiler = require('./compiler.wrapper');

test('Multifns', async () => {
    const wat = await compiler.toWat(`
        defunion: Bool
        : { True False }

        deftype: True
        deftype: False

        defmulti: to-int
        when: True
            drop 1
        when: False
            drop 0

        def: main
        entry: true
        : >True to-int >False to-int =
    `);

    const result = await compiler.run(wat, 'main');

    expect(result.stackElement()).toBe(0);
});

