#!/bin/bash

TEST0="max lock harmonic-e map_filter-e"
TEST1="sum ack hors sum_intro forall_leq fact_notpos-e harmonic search-e"
TEST2="mc91 exc-fact fact_notpos isnil mem a-max fold_left fold_right"
TEST3="nth0 zip"
TEST4="mult search risers"
TEST5="nth"
TEST6=""
TEST7="a-copy-print length"
TEST8=""
TEST_NOPROGRESS=""
TEST_TOHMC="enc-zip_unzip"
TEST_TOPREDABST="copy_intro enc-zipmap file"
TEST_TOPREDDISC="map_filter iter forall_eq_pair fold_fun_list"
TEST="$TEST0 $TEST1 $TEST2 $TEST3 $TEST4 $TEST5 $TEST6 $TEST7 $TEST8 $TEST_NOPROGRESS $TEST_TOHMC $TEST_TOPREDABST $TEST_TOPREDDISC"

LIMIT=100
OPTION="-debug 2"
FPAT_OPTION="-hccs 9 -mode 2"
for i in $TEST
do
echo $i
timeout -s 14 $LIMIT ./mochi.opt test/$i.ml $OPTION -fpat "$FPAT_OPTION" &> $i_exact.log
echo
done





