# Apecs + Hint Demo

Demonstrating how to use the [hint](https://hackage.haskell.org/package/hint) library to dynamically transform the game state of an [apecs](https://hackage.haskell.org/package/apecs)-based game. Run using `stack run`, press `Enter` to run the code from the `new-entity` file, and press `Escape` to quit.

One critical design decision is to put the world and component types in a library, in this case `apecs-hint-demo`, and the code which uses hint outside that library, in this case in an executable stanza. This makes it possible for hint to find that library in a package database and to import a module from that library, making those types available to the interpreted code. It is _not_ possible to use hint to interpret code which uses types defined in the package which calls hint.

The reason I recommend using stack is because stack configures an environment variable which causes hint to load the package database containing the dependencies of your program, in this case both the `apecs` and `apecs-hint-demo` libraries. With cabal, you need to jump through some extra hoops in order to specify the path to the relevant package databases, as explained in the `unsafeRunInterpreterWithArgs` comment.
