#!/bin/bash

TEST="$(ls test/*.ml) $(ls test_cannot_verify/*.ml)"

LIMIT=30
OPTION="-no-exparam"
FPAT_OPTION="-hccs 12 -only-result"
LOG=.ex.log
for i in $TEST
do
echo $i
timeout -s 14 $LIMIT ./mochi.opt $OPTION -fpat "$FPAT_OPTION" $i &> $i$LOG
echo
done
