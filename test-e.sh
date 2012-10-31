#!/bin/sh

TEST="intro1-e.ml intro2-e.ml intro3-e.ml sum-e.ml mult-e.ml mc91-e.ml lock-e.ml repeat-e.ml max-e.ml exception-e.ml"

for i in $TEST
do
  echo $i
  ./mc test/$i
done
