#!/usr/bin/env python3

def summarize_ranges(filename):
    with open(filename, 'r') as file:
        prev_first = None
        prev_second = None
        range_start = None

        for line in file:
            line = line.strip()
            if not line:
                continue
            try:
                first, second = map(int, line.split())
            except ValueError:
                print(f"Skipping invalid line: {line}")
                continue

            if prev_first is None:
                # First line
                prev_first = first
                prev_second = second
                range_start = first
                continue

            if first == prev_first + 1 and second == prev_second:
                # Extend the current range
                prev_first = first
                continue
            else:
                # Print the previous range or single line
                if range_start == prev_first:
                    print(f"{range_start} {prev_second}")
                else:
                    print(f"{range_start}-{prev_first} {prev_second}")
                # Start a new range
                range_start = first
                prev_first = first
                prev_second = second

        # Handle the last range
        if prev_first is not None:
            if range_start == prev_first:
                print(f"{range_start} {prev_second}")
            else:
                print(f"{range_start}-{prev_first} {prev_second}")

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} filename")
    else:
        summarize_ranges(sys.argv[1])
 
