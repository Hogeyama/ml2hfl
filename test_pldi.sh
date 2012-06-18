#!/bin/bash

TEST="intro1 intro2 intro3 neg exc-simple max lock ack hrec sum hors mc91 exc-fact mult zipunzip zipmap repeat a-max a-dotprod a-copy-print a-init sum-e mult-e max-e mc91-e repeat-e lock-e fhnhn"

LIMIT=120
OPTION="-enr -rsn 0"

for i in $TEST
do
#ls -l test_new/$i
echo $i
timeout -s 14 $LIMIT ./mochi.opt test_new/$i.ml $OPTION | grep afe
echo
done
