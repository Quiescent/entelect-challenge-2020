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

function list_tags() {
    git tags -v
}

function backup_scripts() {
    echo "> Creating backup directory..."
    mkdir -p $SCRIPT_BACKUP_DIRECTORY
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

function tracked_by_git() {
    git ls-files --error-unmatch $1
    echo $?
}

function build_bot_if_not_cached() {
    BOT_DIRECTORY=$1
    VERSION=$2

    mkdir -p "$BOT_DIRECTORY"
    cp bot.json "$BOT_DIRECTORY"
    if [ ! -f "$BOT_DIRECTORY/bot" ]; then
        git checkout "$VERSION"
        make
        cp bot "$BOT_DIRECTORY"
    fi
}

function change_bot_name() {
    sed -i "s/Quantum/$2/g" "$1/bot.json"
}

function run_matches() {
    VERSIONS_TO_RUN=$(git tag)
    pushd $GIT_ROOT
    build_bot_if_not_cached "bots/$CURRENT_VERSION" $CURRENT_VERSION
    for BOT_VERSION in $VERSIONS_TO_RUN; do
        build_bot_if_not_cached "bots/$BOT_VERSION" "tags/$BOT_VERSION"
        change_bot_name "bots/$BOT_VERSION" "$BOT_VERSION"
    done
    popd
    git checkout "master"
    python3 run-against-versions.py "$CURRENT_VERSION" $VERSIONS_TO_RUN
}

function main() {
    if [ ! -z "$(git status --porcelain)" ]; then
        echo "Error: Will only run on a clean repo."
        echo "Please commit your changes!"
    elif [[ "$(pwd)" != "$GIT_ROOT"* ]]; then
        echo "Error: Not in the project repo."
        echo "Please run this script from there."
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
