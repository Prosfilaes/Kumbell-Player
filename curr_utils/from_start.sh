#!/bin/sh
sb=sb
size=100000000
export TMPDIR=/var/tmp
../build_from_start_kalah2 --start $sb movebook_1 work_1 || exit 1
mv work_1 new_work_1
for i in $(seq 2 $1); do
        ../build_from_start_kalah2 $sb movebook_$i new_work_$((i - 1)) work_$i
        sort -rn work_$i | uniq | head -n $size > new_work_$i
        rm work_$i
done
