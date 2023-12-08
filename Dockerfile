### dependencies base image ################################

FROM haskell:9.4.8 as dependencies
RUN cabal update
RUN mkdir -p /opt/build
WORKDIR /opt/build
COPY image/dependencies.cabal /opt/build/dependencies.cabal
RUN cabal build




### Build image ############################################

FROM haskell:9.4.8 as build
COPY --from=dependencies /root/.cabal /root/.cabal
RUN mkdir /opt/build
WORKDIR /opt/build

# Build and cache dependencies not listed above
COPY cabal.project nso-level2.cabal .
RUN cabal build --only-dependencies

# Copy in code
COPY *.md .
COPY deps/metadata.graphql deps/metadata.graphql
COPY migrations migrations
COPY app app
COPY test test

# Run tests
RUN cabal test

# Install
RUN cabal install


### runtime image ####################################

FROM ubuntu:22.04
RUN mkdir -p /opt/app
WORKDIR /opt/app

# these will all be cached even if a previous stage has changes
RUN apt-get update -y
RUN apt-get install -y libpq-dev ca-certificates openssl pkg-config
RUN apt-get install -y cargo
RUN cargo install sqlx-cli

COPY image/entrypoint.sh ./entrypoint.sh

COPY --from=build /root/.cabal/bin/level2 .
COPY --from=build /opt/build/migrations ./migrations

ENTRYPOINT ./entrypoint.sh
CMD []
