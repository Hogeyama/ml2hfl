#!/bin/bash

TEST="$(ls test/*.ml) $(ls test_fpat/*.ml)"

LIMIT=600
OPTION="-no-exparam -exp2 -limit $LIMIT"
FPAT_OPTION="-hccs sbe -disable-interp-simp -template 1 -ucore"
LOG=.sbe.log
for i in $TEST
do
echo $i
ulimit -t $LIMIT && ./mochi.opt $OPTION -fpat "$FPAT_OPTION" $i &> $i$LOG
echo
done





