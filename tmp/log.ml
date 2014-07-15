FPAT: option `-hccs' needs an argument.
Options for FPAT are:
  -verbose      Enable verbose mode
  -debug        Enable debug mode
  -hccs <n>     Specify an HCCS solver
		0: dag HCCS solver based on top-down iterative interpolation
		1: dag HCCS solver based on generalization via convex hull
		2: dag HCCS solver based on generalization via template-based synthesis
		3: dag HCCS solver based on generalization via interpolation
		4: dag HCCS solver based on sampling and merging (old implementation)
		5: dag HCCS solver based on sampling and merging + tree HCCS solver based on merging
		6: dag HCCS solver based on sampling and merging + tree HCCS solver based on top-down iterative interpolation
		7: dag HCCS solver based on dag unwinding + tree HCCS solver based on merging
		8: dag HCCS solver based on dag unwinding + tree HCCS solver based on top-down iterative interpolation
		9: complete dag HCCS solver based on relaxed stratification (with cut-len=4)
		10: dag HCCS solver based on top-down iterative interpolation by MathSAT 5
		11: complete dag HCCS solver based on exact L-restriction
		12: complete dag HCCS solver based on relaxed stratification (with cut-len=1)
		13: dag HCCS solver based on forward abstract interpretation with widening
  -interp <n>   Specify an interpolating prover
		0: CSIsat
		1: CSIsat + some generalization heuristics
		2: a templated-based interpolating prover
  -smt <n>      Specify an SMT solver
		0: Z3
		1: CVC3
  -boolenc <n>  Specify an encoding method for booleans
		0: Disable integer encoding
		1: Enable integer encoding (true -> 1, false -> 0)
		2: Enable integer encoding (true -> positive integers, false -> non-positive integers)
  -template <n> Specify a template-based synthesis method
		0: polynomial constraint solving
		1: linear constraint solving (only for dag HCCS)
		2: mixed integer linear programming (only for dag HCCS)
		3: convex quadratic programming (1) (only for dag HCCS)
		4: convex quadratic programming (2) (only for dag HCCS)
		5: convex quadratic programming (3) (only for dag HCCS)
		6: convex quadratic programming (4) (only for dag HCCS)
  -eqelim <n>   Specify an existential quantifier elimination method
		0: polynomial constraint solving
		1: bit-blasting
  -aec          Accumulate the constraints for existential quantifier elimination
  -dph          Disable a heuristics for existential quantifier elimination
  -rank <n>     Specify a ranking function synthesis method
		0: polynomial constraint solving
		1: linear constraint solving (only for HCCS w/o existential quantifiers)
		2: convex quadratic programming (only for HCCS w/o existential quantifiers)
		3: bit-blasting
		4: bit-blasting (only for HCCS w/o existential quantifiers)
  -rbf          Try to synthesize a ranking function with small coefficients
  -tbit         Threshold of the number of bits for bit-blasting
  -wp-max <n>   The maximum dimension of hypercubes in predicate abstraction
  -neg-pred     Use negative predicates for abstraction
  -popl2015     Perform experiments for POPL2015
  -popl2015ex   Perform additional experiments for POPL2015
  -vmcai2015    Perform experiments for VMCAI2015
  -help         Display this list of options
  --help        Display this list of options
