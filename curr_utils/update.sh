#!/bin/sh

set -e

export TMPDIR=/var/tmp/kumbell
#cat movebook_* unsum | sed 's/^[[:space:]]//g;s/[[:space:]]$//g' | sort -n | uniq  > unsum_
cat movebook_* | cut_movebook | sort -n | uniq  > $TMPDIR/unsum_part
touch unsum
sort -n -m unsum $TMPDIR/unsum_part | uniq > $TMPDIR/unsum_
if [ ! -s $TMPDIR/unsum_ ]; then
        echo "failed to generate new unsum"
        exit 1
fi
mv $TMPDIR/unsum_ unsum
s2b unsum sb.bin
rm -f $TMPDIR/unsum_part $TMPDIR/unsum_cut movebook_*  || true
