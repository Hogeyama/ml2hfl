#!/bin/bash

TEST="$(ls test_modular/*.ml)"
TEST="$(echo test_modular/{sum.ml,sum2.ml,zero.ml,mult.ml,sum_mult_mc91.ml,example1.ml,applyl,twice})"


OPTION=" -only-result -ignore-conf -modular"
for i in $TEST
do
echo $i
./mochi.opt $OPTION $i
echo
done
