#!/bin/bash


set -e

echo "MIGRATING $DATABASE_URL"
/root/.cargo/bin/sqlx migrate run

echo "RUNNING"

# use THREADS=1 if it doesn't exist
# docker run -e THREADS=24 --cpuset-cpus="0-23" --cpuset-mems="0" level2
: "${THREADS:=1}"

# exec ./level2 +RTS -N"${THREADS}" -s -RTS
exec ./level2 +RTS -N"${THREADS}" -RTS
