const compiler = require('./compiler.wrapper');

test('Empty', async () => {
    const wat = await compiler.toStringWat('main', `
        def: main
        type: -- /string/String
        : ""
    `);

    const result = await compiler.run(wat, '/author/sample/core/main');

    expect(result.stringElement()).toBe("");
});

test('Sample', async () => {
    const wat = await compiler.toStringWat('main', `
        def: main
        type: -- /string/String
        : "This is a test"
    `);

    const result = await compiler.run(wat, '/author/sample/core/main');

    expect(result.stringElement()).toBe("This is a test");
});

test('Emoji', async () => {
    const wat = await compiler.toStringWat('main', `
        def: main
        type: -- /string/String
        : "💩"
    `);

    const result = await compiler.run(wat, '/author/sample/core/main');

    expect(result.stringElement()).toBe("💩");
});
