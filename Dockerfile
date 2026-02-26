### dependencies base image ################################

FROM haskell:9.6.7 AS dependencies

# SYSTEM BUILD DEPENDENCIES
RUN apt-get update -y

RUN apt-get update -y && apt-get install -y --no-install-recommends \
  ca-certificates curl gnupg lsb-release \
  build-essential libssl-dev pkg-config \
  && rm -rf /var/lib/apt/lists/*

# Add PostgreSQL apt repo (PGDG) so we can install libpq-dev >= 14
RUN install -d /etc/apt/keyrings \
  && curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc \
     | gpg --dearmor -o /etc/apt/keyrings/postgresql.gpg \
  && echo "deb [signed-by=/etc/apt/keyrings/postgresql.gpg] http://apt.postgresql.org/pub/repos/apt \
     $(. /etc/os-release && echo $VERSION_CODENAME)-pgdg main" \
     > /etc/apt/sources.list.d/pgdg.list

# Install newer libpq-dev from PGDG
RUN apt-get update -y && apt-get install -y --no-install-recommends \
  libpq-dev \
  && rm -rf /var/lib/apt/lists/*

# basic cabal dependencies
RUN cabal update
RUN cabal install hpack

RUN mkdir -p /opt/build
WORKDIR /opt/build


# install all cabal files at once, so the solver only goes once
COPY cabal.project package.yaml .
COPY types/package.yaml types/

RUN hpack && hpack types

RUN mkdir app src test deps types/src
RUN touch LICENSE README.md DEVELOPMENT.md deps/metadata.graphql
RUN cabal build --only-dependencies




### Build image ############################################

FROM dependencies AS build
WORKDIR /opt/build


# Copy in all code, respect .dockerignore
COPY . . 

# Run tests
RUN cabal test

# Install
RUN cabal install

### runtime image ####################################

FROM ubuntu:24.04
RUN mkdir -p /opt/app
WORKDIR /opt/app

RUN apt-get update -y
RUN apt-get install -y curl build-essential libssl-dev libpq-dev ca-certificates openssl pkg-config
RUN curl -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo --version
RUN cargo install sqlx-cli


COPY --from=build /root/.local/bin/level2 .
COPY --from=build /opt/build/migrations ./migrations

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

COPY image/entrypoint.sh ./entrypoint.sh
RUN chmod +x ./entrypoint.sh
ENTRYPOINT ["./entrypoint.sh"]
