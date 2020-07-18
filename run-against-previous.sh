#!/usr/bin/env bash

### Commentary
# This script executes the current version of the bot against all
# prevously tagged versions.  It checks whether the bot won in each
# case and aggregates the results, printing them to a table in the
# end.

### Code

set -e

SCRIPT_BACKUP_DIRECTORY=script_bin
GIT_ROOT=$(git rev-parse --show-toplevel)
CURRENT_VERSION=$(git rev-parse HEAD)

source ./build_bots.sh

function backup_scripts() {
    echo "> Creating backup directory..."
    mkdir -p $SCRIPT_BACKUP_DIRECTORY
    cp build_bots.sh script_bin/
    cp run-against-previous.sh script_bin/
    cp run-against-versions.py script_bin/
}

function run_in_backup() {
    echo "> Entering script backup directory..."
    pushd $SCRIPT_BACKUP_DIRECTORY
    ./run-against-previous.sh "running-in-script-bin"
    echo "> Leaving script backip directory..."
    popd
}

function run_matches() {
    # Remove random and nothing from the list of bots and run against
    # the last five bots.
    VERSIONS_TO_RUN=$(git tag | sort -n | tail -5 | grep -v 12 | grep -v 13 | grep -v 14)
    build_bots $VERSIONS_TO_RUN
    python3 run-against-versions.py "$CURRENT_VERSION" $VERSIONS_TO_RUN
}

function main() {
    if [ ! -z "$(git status --porcelain)" ]; then
        echo "Error: Will only run on a clean repo."
        echo "Please commit your changes!"
    # elif [[ "$(pwd)" != "$GIT_ROOT" ]]; then
    #     echo "Error: Not in the project repo."
    #     echo "Please run this script from there."
    elif [ -z "$1" ]; then
        echo "========================================"
        echo "====Welcome to Run Against Previous====="
        echo "========================================"
        echo ""
        echo "> In root of project..."
        echo "> Backing up and running from there..."
        backup_scripts
        run_in_backup
    else
        echo "> Running from backup directory..."
        run_matches
    fi
}

main $1
