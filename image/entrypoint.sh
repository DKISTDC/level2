#!/bin/bash

set -e

echo "MIGRATING $DATABASE_URL"
/root/.cargo/bin/sqlx migrate run

echo "RUNNING"

# Run with all available CPUs
# See CPU_WORKERS to limit number allowed for CPU-intensive tasks
exec ./level2 +RTS -N -RTS
