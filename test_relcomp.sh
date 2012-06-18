#!/bin/bash

TEST="app-leq fhnhn-eq fhnhn-leq app-lin-ord2 app-lin-ord3 intro2 intro3 repeat-add a-max l-forall-leq a-checksum a-test-update app-succ"

LIMIT=120
OPTION="-enr2 -rsn 0 -rc"

for i in $TEST
do
#ls -l relcomp/$i
echo $i
timeout -s 14 $LIMIT ./mochi.opt relcomp/$i.ml $OPTION > $i.log | grep afe
echo
done





