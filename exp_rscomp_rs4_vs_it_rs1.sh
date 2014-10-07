#!/bin/bash

TEST="$(ls test/*.ml) $(ls test_fpat/*.ml)"

LIMIT=300
OPTION="-no-exparam -exp2 -limit $LIMIT"
FPAT_OPTION="-hccs 9 -mode 1"
LOG=.rs4_vs_it_rs1.log
for i in $TEST
do
echo $i
./mochi.opt $OPTION -fpat "$FPAT_OPTION" $i &> $i$LOG
echo
done





