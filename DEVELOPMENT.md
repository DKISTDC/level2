Dependencies
------------

Install dependencies via [GHCUP](https://www.haskell.org/ghcup/)
* cabal 3.10
* ghc 9.8.2
* haskell-language-server 2.9

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






Environment Variables
--------------------

Set the following ENV via a .env file or another method:

```bash
APP_PORT=3000
APP_DOMAIN=localhost
DATABASE_URL=postgres://guest:guest@127.0.0.1:5432/level2
GLOBUS_CLIENT_ID="5422f0b3-e40d-47b1-8c22-f64b1a1ae38e"
GLOBUS_CLIENT_SECRET="..."
GLOBUS_LEVEL2_ENDPOINT=20fa4840-366a-494c-b009-063280ecf70d

SERVICES=MOCK
METADATA_API=http://internal-api-gateway.service.prod.consul/graphql
```




Testing Changes
---------------

Run all dev dependencies, including hpack, docker-compose:nginx and postgres

    $ bin/deps

Run tests and then Run all dev dependencies

    $ bin/dev

Run tests

    $ bin/test

Database Migrations
-------------------

Create a new migration, then edit the generated .sql file

    $ sqlx migrate create

Create your database

    $ sqlx database create

Bring database up-to-date with migrations

    $ sqlx migrate run

Use HTTPS on Localhost
------------------------------

Generate a Self-Signed SSL certificate

```bash
> openssl req -newkey rsa:2048 -new -nodes -x509 -days 3650 -keyout localhost-key.pem -out localhost.pem -addext "subjectAltName = DNS:localhost"
> mkdir ~/certs
> mv *.pem ~/.certs
```

Mac OSX: Drag localhost.pem into your login keychain. Then double-check and change Trust settings to Always Trust

You should be able access https://localhost instead of http://localhost:3000
