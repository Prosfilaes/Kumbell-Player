#!/bin/sh

set -e

export TMPDIR=/var/tmp
#cat movebook_* unsum | sed 's/^[[:space:]]//g;s/[[:space:]]$//g' | sort -n | uniq  > unsum_
cat movebook_* | sed 's/^[[:space:]]//g;s/[[:space:]]$//g' | sort -n | uniq  > unsum_part
sort -n -m unsum unsum_part > unsum_
if [ ! -s unsum_ ]; then
        echo "failed to generate new unsum"
        exit 1
fi
mv unsum_ unsum
cat unsum | cut -f 1-3 -d" "  > unsum_cut
../summarize_ranges.py unsum_cut > sb
rm -f unsum_part unsum_cut movebook_* work_* new_work_* || true
