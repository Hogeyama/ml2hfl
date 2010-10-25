#!/bin/bash

TEST="intro1.ml intro2.ml intro3.ml sum.ml mult.ml max.ml mc91.ml repeat.ml lock.ml exception.ml neg1.ml neg2.ml neg3.ml neg4.ml fxx.ml fhnhn1.ml fhnhn2.ml intro1-e.ml intro2-e.ml intro3-e.ml sum-e.ml mult-e.ml mc91-e.ml lock-e.ml repeat-e.ml max-e.ml"
TEST="intro1.ml intro2.ml intro3.ml sum.ml mult.ml max.ml mc91.ml repeat.ml fhnhn2.ml lock.ml neg4.ml sum-e.ml mult-e.ml max-e.ml mc91-e.ml repeat-e.ml lock-e.ml repeat2.ml"

for i in $TEST
do
  echo $i
  ./mc test/$i | grep afe
  echo
  echo
done
