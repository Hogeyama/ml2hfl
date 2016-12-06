#!/bin/bash

TEST=test_esop2017/*.ml

if [ "$1" = "" ];then
    LIMIT=60s
else
    LIMIT=$1
fi

cat COMMIT
echo Timeout $LIMIT

#OPTION=" -only-result -ignore-conf -modular -horsat2 -base-to-int -fpat '-wp-max 2'"
OPTION="-fpat '-wp-max 2' -no-exparam -bool-init-empty -only-result -base-to-int -abst-list-literal 2 -modular -ignore-conf"
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
