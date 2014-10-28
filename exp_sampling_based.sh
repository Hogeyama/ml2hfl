#!/bin/bash

TEST="$(ls test/*.ml) $(ls test_fpat/*.ml)"

LIMIT=100
OPTION="-no-exparam -limit $LIMIT"
FPAT_OPTION="-hccs sb -template 2 -ucore -wp-max 2"
LOG=.sampling_based.log
for i in $TEST
do
echo $i
./mochi.opt $OPTION -fpat "$FPAT_OPTION" $i &> $i$LOG
echo
done
