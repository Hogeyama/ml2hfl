#!/bin/bash

TEST="$(ls test/*.ml) $(ls test_cannot_verify/*.ml)"

LIMIT=30
OPTION="-no-exparam -exp2"
FPAT_OPTION="-hccs 1"
LOG=.pepm2013.log
for i in $TEST
do
echo $i
timeout -s 14 $LIMIT ./mochi.opt $OPTION -fpat "$FPAT_OPTION" $i &> $i$LOG
echo
done
