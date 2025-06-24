#!/bin/sh
set -e

for i in 37 7 17 5; do 
        echo == $i ==; 
        ./from_start.sh $i; 
        ./update.sh ; 
done
