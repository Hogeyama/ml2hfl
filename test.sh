#!/bin/bash

TEST="$(ls test/*.ml)"

LIMIT=3
OPTION="-limit $LIMIT"
for i in $TEST
do
echo $i
./mochi.opt $OPTION $i -only-result
echo
done
