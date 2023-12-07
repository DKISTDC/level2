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

# Copy in code and compile
COPY *.md .
COPY app app
COPY migrations migrations
COPY deps/metadata.graphql deps/metadata.graphql
RUN cabal build
RUN cabal install


### runtime image ####################################

FROM ubuntu:22.04
RUN mkdir -p /opt/app
WORKDIR /opt/app

RUN apt-get update -y
RUN apt-get install -y libpq-dev ca-certificates

COPY --from=build /root/.cabal/bin/level2 .

ENTRYPOINT ./level2
CMD []
