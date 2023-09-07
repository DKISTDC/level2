Dependencies
------------

Install dependencies via [GHCUP](https://www.haskell.org/ghcup/)
* cabal 3.6+
* ghc 9.2
* haskell-language-server 2.1

    > ghcup tui

Install command-line tools:
* [ghcid](https://github.com/ndmitchell/ghcid)
* [hpack](https://github.com/sol/hpack#readme)

    > cabal update
    > cabal install ghcid
    > cabal install hpack

Testing Changes
---------------

Recompile package.yaml when files are added or changes are made

    > watchexec -e hs,yaml hpack

Testing individual changes

    > cabal repl
    ghci> import NSO.Level2.MyModule
    ghci> runMyFunction

Auto-reload: write a `test` function in your module, then:
  
    > ghcid --test NSO.MyModule.test

Running automated tests

    > cabal test
