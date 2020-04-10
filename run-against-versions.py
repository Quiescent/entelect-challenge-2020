#!/usr/bin/env python

import sys
import json
import subprocess
import os
from pathlib import Path

GAME_RUNNER_DIRECTORY = "../../EntelectChallenge-2020-Overdrive/game-runner"
GAME_RUNNER_MATCH_PATH = "%s/match-logs" % GAME_RUNNER_DIRECTORY
GAME_RUNNER_CONFIG_PATH = "%s/game-runner-config.json" % GAME_RUNNER_DIRECTORY
BOT_PATH_FROM_GAME_RUNNER = "../../entelect-challenge-2020/bots"
MATCHES_TO_RUN = 5


def main(argv):
    """Run the version specified by the first argument against others."""
    if len(argv) < 2:
        usage()
    base_version = argv[1]
    other_versions = argv[2:]
    print("python> Running %s against %s" % (base_version, other_versions))
    match_record = run_against_all(base_version, other_versions)
    print_report(match_record)


def bool_to_description(win_condition):
    """Produce "won" if WIN_CONDITION is true and otherwise "lost"."""
    if win_condition:
        return "won"
    return "lost"


def print_report(match_record):
    """Pretty print the MATCH_RECORD as a table.

Helps one to reason about the results for the match runs."""
    dashes = "-" * 70
    for bot in match_record:
        print("Matches won by %s, against:" % bot)
        print(dashes)
        print('{:^10s}{:<10s}{:<10s}{:<10s}{:<10s}{:<10s}{:>10s}'.format(
            "Bot",
            "Match 1", "Match 2",
            "Match 3", "Match 4",
            "Match 5", "Total"))
        print(dashes)
        for other_bot in match_record[bot]:
            record = match_record[bot][other_bot]
            total_wins = record.count(True)
            print('{:^10s}{:<10s}{:<10s}{:<10s}{:<10s}{:<10s}{:>10}'.format(
                other_bot,
                bool_to_description(record[0]),
                bool_to_description(record[1]),
                bool_to_description(record[2]),
                bool_to_description(record[3]),
                bool_to_description(record[4]),
                total_wins))
        print(dashes)


def run_against_all(base_version, other_versions):
    """Run BASE_VERSION against bots of all OTHER_VERSIONS."""
    match_record = {}
    for other_version in other_versions:
        for _ in range(MATCHES_TO_RUN):
            run_one_match(base_version, other_version, match_record)
    return match_record


def store_match_result(result, bot_1, bot_2, match_record):
    """Store the RESULT of running BOT_1 against BOT_2 in MATCH_RECORD."""
    if match_record.get(bot_1) is None:
        match_record[bot_1] = {}
    if match_record[bot_1].get(bot_2) is None:
        match_record[bot_1][bot_2] = []
    match_record[bot_1][bot_2].append(result)


def check_latest_match_result():
    """Produce TRUE if player 1 won the last run match."""
    paths = sorted(
        Path(GAME_RUNNER_MATCH_PATH).iterdir(),
        key=os.path.getmtime)
    paths.reverse()
    latest_match_dir = paths[0]
    last_round = max((int(dir[6:])
                      for dir in os.listdir(latest_match_dir)
                      if 'Round' in dir))
    last_round_dir = "%s/Round %s" % (latest_match_dir, last_round)
    with open("%s/endGameState.txt" % last_round_dir, 'r') as file:
        for line in file:
            if "The winner is: A - Quantum" in line:
                return True
    return False


def run_one_match(bot_1, bot_2, match_record):
    """Run BOT_1 against BOT_2.

Store the results in MATCH_RECORD."""
    print("Running matches for bots: %s, %s" % (bot_1, bot_2))
    set_bot_root_config(bot_1, bot_2)
    subprocess.run(
        "cd %s && make run" % GAME_RUNNER_DIRECTORY,
        shell=True,
        check=True)
    result = check_latest_match_result()
    store_match_result(result, bot_1, bot_2, match_record)


def set_bot_root_config(bot_1, bot_2):
    """Set the config to run a match between BOT_1 and BOT_2.

Open the file `game-runner-config.json`, parse the json and set the
coressponding values."""
    config = {}
    with open(GAME_RUNNER_CONFIG_PATH, 'r') as file:
        config = json.loads(file.read())
        config["player-a"] = "%s/%s" % (BOT_PATH_FROM_GAME_RUNNER, bot_1)
        config["player-b"] = "%s/%s" % (BOT_PATH_FROM_GAME_RUNNER, bot_2)
    with open(GAME_RUNNER_CONFIG_PATH, 'w') as file:
        file.write(json.dumps(config, sort_keys=True, indent=4))
        file.close()


def usage():
    """Print a usage help string."""
    sys.stderr.write("Usage:\n")
    sys.stderr.write("\t./run-against-versions <main-version> <other-version> [other-versions..]\n")
    sys.exit(1)


if __name__ == "__main__":
    main(sys.argv)
