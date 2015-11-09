#!/bin/bash
dir=`dirname "$0"`
prove -fcm --timer --trap --exec "${dir}/test_fair_non_termination/test.py" ${dir}/test_fair_non_termination/*.ml
