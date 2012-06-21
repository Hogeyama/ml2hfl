#!/bin/bash

TEST="app-leq fhnhn-eq fhnhn-leq app-lin-ord2 app-lin-ord3 intro2 l-forall-leq app-succ intro3 repeat-add a-max a-checksum a-test-update"

LIMIT=120
OPTION="-enr2 -rsn 128 -rc"

for i in $TEST
do
#ls -l relcomp/$i
echo $i
timeout -s 14 $LIMIT ./mochi.opt relcomp/$i.ml $OPTION | grep afe
echo
done





