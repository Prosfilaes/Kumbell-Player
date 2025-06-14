#!/usr/bin/env python3

import sys
import re

def desummarize_line(line):
    line = line.strip()
    if not line:
        return

    # Try range line: "1-3 5"
    match = re.match(r'^(\d+)-(\d+)\s+(\d+)$', line)
    if match:
        start, end, value = map(int, match.groups())
        for i in range(start, end + 1):
            print(f"{i} {value}")
        return

    # Try single line: "7 9"
    match = re.match(r'^(\d+)\s+(\d+)$', line)
    if match:
        print(line)
        return

    print(f"Invalid line: {line}", file=sys.stderr)

def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} summarized_input.txt", file=sys.stderr)
        sys.exit(1)

    with open(sys.argv[1], 'r') as f:
        for line in f:
            desummarize_line(line)

if __name__ == "__main__":
    main()

