#!/usr/bin/env bash

### Commentary
# This script executes the current version of the bot against all
# prevously tagged versions.  It checks whether the bot won in each
# case and aggregates the results, printing them to a table in the
# end.

### Code

set -e

SCRIPT_BACKUP_DIRECTORY=script_bin

function list_tags() {
    git tags -v
}

function backup_scripts() {
    echo "> Creating backup directory..."
    mkdir $SCRIPT_BACKUP_DIRECTORY
    cp run-against-previous.sh script_bin/
    cp run-against-versions.py script_bin/
}

function run_in_backup() {
    echo "> Entering script backup directory..."
    pushd $SCRIPT_BACKUP_DIRECTORY
    # ./run-against-previous.sh
    echo "> Leaving script backip directory..."
    popd
}

function main() {
    echo "0: $0"
    if [ "$0" == "$(git rev-parse --show-toplevel)" ]; then
        echo "========================================"
        echo "====Welcome to Run Against Previous====="
        echo "========================================"
        echo ""
        echo "> In root of project..."
        echo "> Backing up and running from there..."
        backup_scripts
        run_in_backup
    else
        echo "> In backup already..."
    fi
}

main
