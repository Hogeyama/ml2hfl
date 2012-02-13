How to install
--------------
Run "make trecs"

How to run
-----------

  trecs [option] <filename>

 option: 
    -st    do type checking before model checking
    -p <num1> <num2>   control search depth
    -o <filename>
          Output the result to <filename>.
          The output is either:
             SATISFIED  
                  if the property is satisfied; or
             VIOLATED
             ... (error trace)
                  if the property is not satisfied.
          The format of an error trace is 
              (a_1,d_1)(a_2,d_2)....(a_n,d_n).
          where a_i is a terminal symbol, and 
                d_i is a non-negative integer representing the direction of the branch.

Syntax of Input File
------------------
The source file should be of the following form:

%BEGING
 Definition of a recursion scheme, consisting of rewriting rules of the form "F x1 ... xn -> t."
 The non-terminal on the lefthand side of the first rule is interpreted as the start symbol.
%ENDG
%BEGINA
 Definition of a trivial automaton, consisting of transition rules of the form "q a -> q1 ... qn."
 The state on the lefthand side of the first rule is interpreted as the initial state.
%ENDA


The syntax of terms is:

  t ::= x   (variable)
        c   (terminal symbol)
        F   (non-terminal)
        t1 t2
        (t1,t2)           (pair constructor)
        _dcons t1 t2     (pair destructor; t1 is a pair of type A*B, and t2 is a continuation of tyep A->B->o)
        i    (non-negative integer)
        _case n t t1 ... tn   (case branch: reduced to ti if t is i)

See examples in the directory "examples" for details.

