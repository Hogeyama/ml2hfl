# Non-terminating
ulimit -t20 | ./mochi.opt -non-termination -fpat "-eahccs 3" test_infer/fib_nonterm.ml
ulimit -t20 | ./mochi.opt -non-termination -fpat "-eahccs 3" test_infer/loop.ml
# Unknown
ulimit -t20 | ./mochi.opt -non-termination -fpat "-eahccs 3" test_infer/terminate.ml
ulimit -t20 | ./mochi.opt -non-termination -fpat "-eahccs 3" test_infer/terminate2.ml
