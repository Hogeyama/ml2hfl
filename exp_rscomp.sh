#!/bin/bash

LIMIT=300
OPTION=""
FPAT_OPTION="-hccs 11 -mode 1"
for i in $(ls test/*.ml)
do
echo $i
timeout -s 14 $LIMIT ./mochi.opt $i $OPTION -fpat "$FPAT_OPTION" &> $i.log
echo
done





