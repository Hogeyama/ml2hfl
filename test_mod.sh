#!/bin/bash

TEST="$(ls test_modular/*.ml)"
#TEST="$(echo test_modular/{sum.ml,sum2.ml,zero.ml,mult.ml,sum_mult_mc91.ml,example1.ml,apply.ml,twice.ml,twice_cps.ml})"

if [ "$1" = "" ];then
    LIMIT=60s
else
    LIMIT=$1
fi

cat COMMIT
echo Timeout $LIMIT

OPTION=" -only-result -ignore-conf -modular -horsat2 -base-to-int -fpat '-wp-max 2'"
for i in $TEST
do
    echo
    printf "%0.s=" $(seq $(tput cols))
    echo
    echo
    echo $i
    echo
    echo $OPTION | xargs timeout $LIMIT ./mochi.opt $i || echo 'TIMEOUT OR ERROR'
done
