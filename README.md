# Lambda-Calc-Compiler
## Lambda Calculus Compiler, REPL, Interpreter, &amp; Library!

 ----------------------------------------------------------------------------
# C++ Library:
### NAMESPACE `LambdaCalc` LAMBDAS
* _ALL DATA IS IMMUTABLE (`CONST`)_
* _ALL LAMBDAS ARE CURRIED ( IE `Add(ox1, ox2)` => `Add(ox1)(ox2)` )_
* _CAPTURE SCOPE BY ***VALUE*** (using `[=]`) FOR INNER - CURRIED! - LAMBDAS_

### NOTATION:
* _let N = Church Numeral, B = Fcnal Bool, F1 = Unary Fcn, F2 = Binary Fcn,</br>
      F = arbitrary fcn, X = arbitrary data,</br>
      L = Fcnal List Data Structure (See More Below)_

 ----------------------------------------------------------------------------
### VISUALIZATION:

 * `show(X)`   => _print arbitrary data x to screen + newline_
 * `print(X)`  => _print arbitrary data x to screen_

 * `bshow(B)`  => _print fcnal boolean as boolean boolean + newline_
 * `bprint(B)` => _print fcnal boolean as boolean boolean_

 * `nshow(N)`  => _print church numeral as `unsigned long long` + newline_
 * `nprint(N)` => _print church numeral as `unsigned long long`_

 ----------------------------------------------------------------------------
### FCNAL BOOLEANS:

#### EXPLANATION:
* Fcnal Booleans are Binary Fcns, acting like C++'s Ternary `?:` operator: 
  * Fcnal `True` chooses arg1, `False` chooses arg2
 
#### BOOLEANS & BOOLEAN OPERATIONS:
* `True`
* `False`
* `Not (B)`
* `And (B1)(B2)`
* `Or  (B1)(B2)`
* `Xor (B1)(B2)`
* `Beq (B1)(B2)` => _'B'oolean 'eq'uality, ie xnor_
 
 ----------------------------------------------------------------------------
### CHURCH-NUMERAL NUMERIC FCNS:

#### EXPLANATION:
* N-Fold Compositions of a Fcn (!!! ALL >= Zero Integers !!!):
  * IE `Zero` = `a`, `Once` = `f(a)`, `Twice` = `f(f(a))`,  `Thrice` = `f(f(f(a)))`, etc

#### NUMERALS:
* `Zero`, `Once`, `Twice`, `Thrice`, `Fourfold`, `Fivefold`
* `ox0`,`ox1`,`ox2`,`ox3`,`ox4`,`ox5`,`ox6`,`ox7`,`ox8`,`ox9`,`oxa`,`oxb`,`oxc`,`oxd`,`oxe`,`oxf`

#### COMPARATIVE BOOLEANS:
* `Is0 (N)` => _Equal to 'Zero'_

* `Eq  (N1)(N2)` => _Equal-to_
* `Lt  (N1)(N2)` => _Less Than_
* `Gt  (N1)(N2)` => _Greater Than_
* `Leq (N1)(N2)` => _Less Than Or Equal-to_
* `Geq (N1)(N2)` => _Greater Than Or Equal-to_
 
* `IsFactor (N1)(N2)` => _N1 is a factor of N2_
* `Evenp    (N)`      => _N is even_
* `Oddp     (N)`      => _N is odd_
 
#### ARITHMETIC:
* `Add  (N1)(N2)` => _N1 + N2_
* `Sub  (N1)(N2)` => _N1 - N2_
* `Mult (N1)(N2)` => _N1 * N2_
* `Pow  (N1)(N2)` => _N1 ** N2_

* `Succ (N)` => _Succesor of N,    N+1_
* `Pred (N)` => _Predecessor of N, N-1_

* `Factorial       (N)`      => _N! (w/o Loops, Recursion, or Mutability!!!)_
* `NumericSum      (N)`      => _Sum (0,N)_
* `NumericSumRange (N1)(N2)` => _Sum (N1,N2)_
 
 ----------------------------------------------------------------------------
### PURELY-FCNAL LIST DATA-STRUCTURE FCNS:
 
 #### CONSTRUCTION (Given List Size):
* `ListN(N) (N1) (N2) (N3) (NN)` => _Returns List size N of the trailing elts_

 #### BASIC ANALYSIS:
* `Length (L)` => _Returns length of L_
* `Nullp  (L)` => _"Null" 'p'redicate: List is EMPTY_
* `Pairp  (L)` => _"Pair" 'p'redicate: List is __NOT__ EMPTY_

 #### GETTERS:
* `Head (L)`    => _Return L's 1st cell value_
* `Last (L)`    => _Return L's last cell value_
* `Nth  (N)(L)` => _Returns L's 'N'th elt (starting from 'ox1')_
 
 #### SETTERS: 
* `Push (X)(L)` => _Returns List w/ X in front of L_
* `Pop  (L)`    => _Returns List w/o L's Head_
  * ***NOTE**: "_back" versions may be self-implemented via "`Backward`" fcn (More Below)*
 
 #### FILTER/MAP/VOIDMAP:
* `Filter  (F1)(L)` => _Returns List having filtered out elts from L __NOT__ passing F1_
* `Map     (F1)(L)` => _Returns List having mapped F1 across all of L's elts_
* `VoidMap (F1)(L)` => _Returns Void & F1 Must be void, Apply Fcn to each elt in L_</br>
  * ***NOTE** `VoidMap` helps passing "printer" fcns (ie `nshow`) to print each elt*

 #### REVERSED LIST & FCN APPLICATION:
* `Reverse        (L)`     => _Returns List of L in reverse_
* `FlipArgs       (F2)`    => _Flips args for a Binary Fcn_
* `Backward       (F1)(L)` => _Returns List having applied F on `Reverse(L)`_
* `BackwardAtomic (F1)(L)` => _Returns Atom (IE Non-List Fcn) having applied F on `Reverse(L)`_

 #### ACCUMULATORS:
* `Foldl (F2)(X)(L)` => _Applies F2 on L from 'l'eft to right, starting w/ 'X' & `Head(L)`_
* `Foldr (F2)(X)(L)` => _Applies F2 on L from 'r'ight to left, starting w/ 'X' & `Last(L)`_
 
 #### MAX/MIN:
* `Max (L)` => _Returns Greatest value in List_
* `Min (L)` => _Returns Smallest value in List_

 #### LISP-STYLE ACCESS:
* `car    (L)` => _Returns Current Cell Value ( IE Head(L) )_
* `cdr    (L)` => _Returns Next Cell ( IE Pop(L) )_
* `cadr   (L)` => _Head(Pop(L))_
* `caddr  (L)` => _Head(Pop(Pop(L)))_
* `cadddr (L)` => _Head(Pop(Pop(Pop(L))))_

 ----------------------------------------------------------------------------
### IF YOU'VE GOTTEN THIS FAR ...

#### You may genuinely enjoy the 2 JS Lambda Calculus videos below, found at: 
* Part 1: https://www.youtube.com/watch?v=3VQ382QG-y4&feature=youtu.be
* Part 2: https://www.youtube.com/watch?v=pAnLQ9jwN-E
 
#### In Summary:
* *Identity/Once,     Idiot*:      `I := \a.a`
* *First/True/Const,  Kestrel*:    `K := \ab.a`
* *Flip/LogicalNot,   Cardinal*:   `C := \fab.fba`
* *Unary Compose,     Bluebird*:   `B := \fga.f(ga)`

* *Self-Replication,  Mockingbird* `M := \f.f(f)` => ___IMPOSSIBLE IN HASKELL (Infinite Data Struct)___

* *Second/False/Zero, Kite*:       `KI := \ab.b        = K I = C K`
* *Binary Compose,    Blackbird*:  `B1 := \fgab.f(gab) = B B B`
* *Hold An Arg,       Thrush*:     `Th := \af.fa       = C I`
* *Hold Arg Pair,     Vireo*:      `V  := \abf.fab     = B C Th = B C (C I)`