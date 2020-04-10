#!/usr/bin/env python

import sys

def main(argv):
    """Run the version specified by the first argument against others."""
    if len(argv) < 2:
        usage()
    print("Ran!")

def usage():
    """Print a usage help string."""
    sys.stderr.writelines("""Usage:
./run-against-versions <main-version> <other-version> [other-versions..]""")


if __name__ == "__main__":
    main(sys.argv)
