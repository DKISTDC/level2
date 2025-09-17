Dependencies
------------

Install dependencies via [GHCUP](https://www.haskell.org/ghcup/). Use recommended

    > ghcup tui

Install command-line tools:
* [ghcid](https://github.com/ndmitchell/ghcid)
* [hpack](https://github.com/sol/hpack#readme)

    > cabal update
    > cabal install ghcid
    > cabal install hpack
p
Postgres Database
* Install [Docker](https://www.docker.com/get-started/)
* Install [SQLX](https://github.com/launchbadge/sqlx/blob/main/sqlx-cli/README.md) (migrations) 

    > brew install sqlx-cli


Environment Variables
--------------------

Set the following ENV via a .env file or another method:

```bash
APP_DOMAIN=localhost
DATABASE_URL=postgres://dev:dev@localhost:5432/level2
GLOBUS_ADMIN_TOKEN=AgjbWBl8Ol24DB4MoJlqpDzOQ2YQ2D90oVEqW1zWQm6wY294PJF9C4Vz1EDe1yv9Ebkz87jvWJ85xwIO8lBjdUKV0PK
# SERVICES=MOCK
METADATA_STORE_API_LOCAL=../dkistdc/metadata-store-api/
FRAME_CATALOGER_LOCAL=../dkistdc/frame-cataloger/
METADATA_API_DATASETS=MOCK
# METADATA_API_DATASETS=http://localhost:8080/graphql
# METADATA_API_DATASETS=http://internal-api-gateway.service.prod.consul/graphql
# METADATA_API_INVERSIONS=http://internal-api-gateway.service.prod.consul/graphql
METADATA_API_INVERSIONS=http://localhost:8080/graphql
SCRATCH_DIR=/Users/seanhess/Data
SCRATCH_GLOBUS_DIR=Data
GLOBUS_CLIENT_ID=5422f0b3-e40d-47b1-8c22-f64b1a1ae38e
GLOBUS_CLIENT_SECRET=AAAAAAAAAAAAAAA08vzYy6p6NliplCjuJrBnnRdeWmY=
GLOBUS_LEVEL2_ENDPOINT=03232d38-5e57-11ef-b967-17fffa478f3e    # TARDIGRADE
#GLOBUS_LEVEL2_ENDPOINT=20fa4840-366a-494c-b009-063280ecf70d   # ASGARD
CPU_WORKERS=4
```


Running the App
---------------

Run all dev dependencies, including hpack, docker-compose:nginx and postgres

    $ bin/deps

Run tests and then Run all dev dependencies

    $ bin/dev

Run only tests

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
