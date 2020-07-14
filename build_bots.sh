#!/usr/bin/env bash

### Commentary
# This script executes the current version of the bot against all
# prevously tagged versions.  It checks whether the bot won in each
# case and aggregates the results, printing them to a table in the
# end.

### Code

set -e

GIT_ROOT=$(git rev-parse --show-toplevel)
CURRENT_VERSION=$(git rev-parse HEAD)

function build_bot_if_not_cached() {
    BOT_DIRECTORY=$1
    VERSION=$2

    mkdir -p "$BOT_DIRECTORY"
    cp bot.json "$BOT_DIRECTORY"
    if [ ! -f "$BOT_DIRECTORY/bot" ]; then
        git checkout "$VERSION"
        make
        cp bot "$BOT_DIRECTORY"
        cp -f score-config "$BOT_DIRECTORY"
    fi
}

function change_bot_name() {
    sed -i "s/Quantum/$2/g" "$1/bot.json"
}

function build_bots() {
    VERSIONS_TO_BUILD="$*"
    pushd $GIT_ROOT
    build_bot_if_not_cached "bots/$CURRENT_VERSION" $CURRENT_VERSION
    for BOT_VERSION in $VERSIONS_TO_BUILD; do
        build_bot_if_not_cached "bots/$BOT_VERSION" "tags/$BOT_VERSION"
        change_bot_name "bots/$BOT_VERSION" "$BOT_VERSION"
    done
    popd
    git checkout "master"
}

if [ "$1" == "main" ]; then
    VERSIONS_TO_BUILD=$(git tag | sort -n)
    echo "Building $VERSIONS_TO_BUILD..."
    build_bots "$VERSIONS_TO_BUILD"
fi
