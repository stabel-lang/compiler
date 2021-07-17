test('Internal package word', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'author/sample',
                module: 'internal',
                source: `
                    def: inc
                    : 1 +

                    defmulti: flip
                    type: Int -- Int
                    : Int( value 1 )
                      drop 0
                    else: drop 1
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: `
                    def: main
                    : 0 
                      internal/flip
                      internal/inc
                `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(2);
});
