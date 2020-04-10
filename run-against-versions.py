#!/usr/bin/env python

import sys

def main(argv):
    """Run the version specified by the first argument against others."""
    if len(argv) < 2:
        usage()
    print("Ran!")

def usage():
    """Print a usage help string."""
    sys.stderr.write("Usage:\n")
    sys.stderr.write("\t./run-against-versions <main-version> <other-version> [other-versions..]\n")
    sys.exit(1)


if __name__ == "__main__":
    main(sys.argv)
