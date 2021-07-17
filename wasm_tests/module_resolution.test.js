test('Internal package functions', async () => {
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

test('Internal package type', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'author/sample',
                module: 'mod',
                source: `
                    defunion: TipeU a
                    : Tipe
                    : TipeGen a

                    defstruct: Tipe

                    defstruct: TipeGen a
                    : value a
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: `
                    defstruct: Todo
                    : marker mod/Tipe

                    def: main
                    : mod/Tipe
                      1 mod/>TipeGen
                      mod/Tipe
                      2 mod/>TipeGen
                      drop-tipes
                      tipe-int

                    def: drop-tipes
                    type: (mod/TipeGen a) mod/Tipe (mod/TipeU a) --
                    : drop drop drop

                    defmulti: tipe-int
                    type: (mod/TipeU Int) -- Int
                    : mod/TipeGen
                      mod/value>
                    : mod/Tipe
                      drop 100
                `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(100);
});

test('External package functons', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'external/sample',
                module: 'external',
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
                      /external/flip
                      /external/inc
                `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(2);
});

test('External package types', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'external/pack',
                module: 'mod',
                source: `
                    defunion: TipeU a
                    : Tipe
                    : TipeGen a

                    defstruct: Tipe

                    defstruct: TipeGen a
                    : value a
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: `
                    defstruct: Todo
                    : marker /mod/Tipe

                    def: main
                    : /mod/Tipe
                      1 /mod/>TipeGen
                      /mod/Tipe
                      2 /mod/>TipeGen
                      drop-tipes
                      tipe-int

                    def: drop-tipes
                    type: (/mod/TipeGen a) /mod/Tipe (/mod/TipeU a) --
                    : drop drop drop

                    defmulti: tipe-int
                    type: (/mod/TipeU Int) -- Int
                    : /mod/TipeGen
                      /mod/value>
                    : /mod/Tipe
                      drop 200
                `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(200);
});
