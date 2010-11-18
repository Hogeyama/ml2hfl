#!/bin/bash

TEST="kmp.ml fact_exn.ml array_init.ml array_max.ml file.ml rev_append.ml rev_accum.ml rev_tricky.ml map_map.ml ack.ml recursive.ml intro1.ml intro2.ml intro3.ml sum.ml mult.ml max.ml mc91.ml repeat.ml fhnhn2.ml lock.ml neg4.ml sum-e.ml mult-e.ml max-e.ml mc91-e.ml repeat-e.ml lock-e.ml repeat2.ml"

TEST="intro1.ml intro2.ml intro3.ml sum.ml mult.ml max.ml mc91.ml ack.ml repeat.ml fhnhn.ml hrec.ml neg.ml a-prod.ml a-cppr.ml l-zipunzip.ml l-zipmap.ml hors.ml e-simple.ml e-fact.ml r-file.ml sum-e.ml mult-e.ml max-e.ml mc91-e.ml repeat-e.ml lock-e.ml excep-e.ml"
TEST="intro1.ml intro2.ml intro3.ml sum.ml mult.ml max.ml mc91.ml ack.ml repeat.ml fhnhn.ml hrec.ml neg.ml a-prod.ml a-cppr.ml a-init.ml l-zipunzip.ml l-zipmap.ml hors.ml e-simple.ml e-fact.ml r-lock.ml r-file.ml sum-e.ml mult-e.ml max-e.ml mc91-e.ml repeat-e.ml lock-e.ml excep-e.ml"

for c in `seq 11`
do
for i in $TEST
do
 wc -w test_paper/$i
done
echo
done
