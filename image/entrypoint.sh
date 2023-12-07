#!/bin/bash

echo "MIGRATING"
~/.cargo/bin/sqlx migrate run

echo "RUNNING"
./level2
