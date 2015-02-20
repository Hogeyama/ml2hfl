#!/bin/bash

TEST=../test_ref_to_assert/*.ml

LIMIT=100
OPTION="-exp"
LOG=pro.log

echo > $LOG
for i in $TEST
do
echo $i
./mochi.opt $OPTION $i | tee -a $LOG
echo
done
