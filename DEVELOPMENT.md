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

Postgres Database
* Install [Docker](https://www.docker.com/get-started/)
* Install [SQLX](https://github.com/launchbadge/sqlx/blob/main/sqlx-cli/README.md) (migrations) 

    > brew install sqlx-cli

Testing Changes
---------------

Run all dev dependencies, including hpack, docker-compose, and ghcid

    $ bin/dev

Run tests

    $ bin/test

Database Migrations
-------------------

Create a new migration, then edit the generated .sql file

    $ sqlx migrate create

Bring database up-to-date with migrations

    $ sqlx migrate run
