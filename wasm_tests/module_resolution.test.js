const compiler = require('./compiler.wrapper');

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

test('Module aliases', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'author/external',
                module: 'tipe',
                source: `
                    defstruct: Tipe
                `
            },
            {
                package: 'author/external',
                module: 'mod',
                source: `
                    def: add
                    : +
                `
            },
            {
                package: 'author/sample',
                module: 'internal/tope',
                source: `
                    defstruct: Tope
                `
            },
            {
                package: 'author/sample',
                module: 'internal/mod',
                source: `
                    def: value
                    : 2
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: `
                    defmodule:
                    alias: ext /mod
                    alias: tope internal/tope
                    :

                    defstruct: TipeTope
                    : first tope/Tope

                    def: main
                    alias: internal internal/mod
                    : 6 
                      internal/value
                      ext/add

                    def: noop
                    alias: tipe /tipe
                    type: tipe/Tipe tope/Tope --
                    : drop drop

                    defmulti: as-int
                    alias: tipe /tipe
                    : tipe/Tipe
                      drop 1
                    : tope/Tope
                      drop 2
                `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(8);
});

test('Module imports', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'author/external',
                module: 'tipe',
                source: `
                    defstruct: Tipe
                `
            },
            {
                package: 'author/external',
                module: 'mod',
                source: `
                    def: add
                    : +
                `
            },
            {
                package: 'author/sample',
                module: 'internal/tope',
                source: `
                    defstruct: Tope
                `
            },
            {
                package: 'author/sample',
                module: 'internal/mod',
                source: `
                    def: value
                    : 2
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: `
                    defmodule:
                    import: /mod add
                    import: internal/tope
                    :

                    defstruct: TipeTope
                    : first Tope

                    def: main
                    import: internal/mod
                    : 6 
                      value
                      add

                    def: noop
                    import: /tipe
                    type: Tipe Tope --
                    : drop drop

                    defmulti: as-int
                    import: /tipe
                    : Tipe
                      drop 1
                    : Tope
                      drop 2
                `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(8);
});

test('Anonymous module exposes everything', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'author/external',
                module: 'ext',
                source: `
                    def: fn1
                    : 1 2 +
                `
            },
            {
                package: 'author/sample',
                module: 'internal',
                source: `
                    def: fn2
                    : 3 5 +
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: `
                    def: main
                    : /ext/fn1 internal/fn2 +                 `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(11);
});

test('Defined module requires explicit exports', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'author/external',
                module: 'ext',
                source: `
                    defmodule:
                    expose: fn1
                    :

                    def: fn1
                    : 1 2 +
                `
            },
            {
                package: 'author/sample',
                module: 'internal',
                source: `
                    defmodule:
                    expose: fn2
                    :

                    def: fn2
                    : 3 5 +
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: `
                    def: main
                    : /ext/fn1 internal/fn2 +                 `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(11);
});

test('Implicit import on stdlib in anonymous modules', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'stabel/standard_library',
                module: 'core',
                source: `
                    def: over
                    type: a b -- a b a
                    : swap dup rotate
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: `
                    def: main
                    : 1 2 over + +
                `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(4);
});

test('Implicit import on stdlib in defined modules', async () => {
    const entryPoint = '/author/sample/core/main'
    const wat = await compiler.toProjectWat({
        entryPoint: entryPoint, 
        modules: [
            {
                package: 'stabel/standard_library',
                module: 'core',
                source: `
                    def: over
                    type: a b -- a b a
                    : swap dup rotate
                `
            },
            {
                package: 'author/sample',
                module: 'core',
                source: `
                    defmodule:
                    expose: main
                    :

                    def: main
                    : 1 2 over + +
                `
            },
        ]
    });

    const result = await compiler.run(wat, entryPoint);

    expect(result.stackElement()).toBe(4);
});
