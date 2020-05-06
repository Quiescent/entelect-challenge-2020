#!/usr/bin/env bash

for bot in $(ls bots); do
    rm -rf "${bot}/rounds"
done
