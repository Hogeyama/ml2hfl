#!/bin/bash

TEST0="max lock harmonic-e map_filter-e"
TEST1="sum ack hors sum_intro forall_leq zip fact_notpos-e"
TEST2="mc91 exc-fact fact_notpos isnil mem fold_left fold_fun_list"
TEST3="nth0 a-max"
TEST4="mult search"
TEST5="nth"
TEST6=""
TEST7="a-copy-print"
TEST8=""
TEST_NOPROGRESS="harmonic risers"
TEST_TOHMC="enc-zip_unzip"
TEST_TOPREDABST="copy_intro enc-zipmap file length"
TEST_TOPREDDISC="map_filter fold_right iter forall_eq_pair"
TEST_TCERROR="search-e"
TEST="$TEST0 $TEST1 $TEST2 $TEST3 $TEST4 $TEST5 $TEST6 $TEST7 $TEST8 $TEST_NOPROGRESS $TEST_TOHMC $TEST_TOPREDABST $TEST_TOPREDDISC"

LIMIT=100
OPTION="-rscomp 0 -popl2015exp -popl2015exact -debug 2"
for i in $TEST
do
echo $i
timeout -s 14 $LIMIT ./mochi.opt test/$i.ml $OPTION &> exact_$i.log
echo
done





