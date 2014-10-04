#!/bin/bash

TEST="$(ls test/*.ml) $(ls test_cannot_verify/*.ml)"

LIMIT=300
OPTION="-no-exparam -exp2"
FPAT_OPTION="-hccs 11 -mode 1"
LOG=.rs_vs_it.log
for i in $TEST
do
echo $i
timeout -s 14 $LIMIT ./mochi.opt $OPTION -fpat "$FPAT_OPTION" $i &> $i$LOG
echo
done





