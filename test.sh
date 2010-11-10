#!/bin/bash

TEST="kmp.ml fact_exn.ml array_init.ml array_max.ml file.ml rev_append.ml rev_accum.ml rev_tricky.ml map_map.ml ack.ml recursive.ml intro1.ml intro2.ml intro3.ml sum.ml mult.ml max.ml mc91.ml repeat.ml fhnhn2.ml lock.ml neg4.ml sum-e.ml mult-e.ml max-e.ml mc91-e.ml repeat-e.ml lock-e.ml repeat2.ml"

ulimit -t 10

for i in $TEST
do
  echo $i
  ./mc test/$i | grep afe
  echo
  echo
done
