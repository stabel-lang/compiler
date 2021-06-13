Stabel is a pure concatinative programming language, which compiles to web assembly.

To initialize a project, create a folder and run this command inside that folder: `stabel init author/projectname`

You now have a `stabel.json` file which describes several aspects of your project. Here you can define the name of the project, the version of the project, any dependencies, where those dependencies can be found and which modules you're exposing outside the project (if you're making a library).

By default, you'll find code in your `src` folder, there should be a simple module there now.

To compile your project, you can run: `stabel compile`

To compile and execute a function, you can run `stabel run /author/projectname/path/to/module/fn` 

Functions executed in this way requires a type signature of `-- Int`.
