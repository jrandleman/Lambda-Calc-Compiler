# Lambda-Calc-Compiler
## Lambda Calculus JS Compiler, Interpreter, REPL, &amp; C++ Library!
 ----------------------------------------------------------------------------
# JavaScript `LCC.js` Compiler, REPL, & Interpreter:
## Compilation Prefix:
0) _Compiled Lambda Calc Files have an `LCC_` Prefix & become native JavaScript_
1) _See [JS_LambdaCalc_SampleExec.js](https://github.com/jrandleman/Lambda-Calc-Compiler/blob/master/JS-SampleExec/JS_LambdaCalc_SampleExec.js)'s compilation into [LCC_JS_LambdaCalc_SampleExec.js](https://github.com/jrandleman/Lambda-Calc-Compiler/blob/master/JS-SampleExec/LCC_JS_LambdaCalc_SampleExec.js) for more!_

## Using the REPL (_Launches by Default!_):
#### REPL COMMANDS (case-insensitive):
 0) `lci> EXIT` _exit REPL_
 1) `lci> SYNTAX-HELP` _show Lambda Calculus syntax guide_
 2) `lci> HELP` _show this message_
#### REPL FILE COMPILATION & LOADING:
 0) `lci> LCC filename` _compile + exit_
 1) `lci> LCI filename` _interpret/evaluate + exit_
 2) `lci> COMPILE filename` _compile_
 3) `lci> LOAD filename` _interpret & load into REPL's history buffer_
#### REPL HISTORY MANIPULATION:
 0) `lci> SHOW-HISTORY` _print REPL history_
 1) `lci> SAVE-HISTORY filename` _save REPL history to "filename"_
 2) `lci> CLEAR-HISTORY` _clear REPL history_
 3) `lci> DELETE lineNumber` _delete code at "lineNumber"_
 4) `lci> REPLACE lineNumber newCode` _rewrite "lineNumber" w/ "newCode"_
 5) `lci> INSERT lineNumber newCode` _insert "newCode" **PRIOR** "lineNumber"_
#### REPL DEFAULT LIBRARY:
 0) `lci> LOAD-LIB` _load lambdas from [JS_LambdaCalc_SampleExec.js](https://github.com/jrandleman/Lambda-Calc-Compiler/blob/master/JS-SampleExec/JS_LambdaCalc_SampleExec.js)_
  1) `lci> LOAD-PRINT` _load printing `show` lambdas_
  2) `lci> LOAD-CHURCH` _load church numeral lambdas_
#### REPL CODING:
 0) `lci> @JS your_JS_expr` _run "your_JS_expr" as JavaScript_
 1) `lci> your_LC_expr` _interpret & run your Lambda Calculus!_
----------------------------------------------------------------------------
## Lambda Calculus REPL Notation & Information:
### RESERVED CHARS: 
 0) _WHITESPACE + `. \ ( ) @`_
 
### NAMING:
 0) ___LAMBDAS__ START W/ __CAPITAL LETTER__ & ONLY HAVE __ALPHANUMERICS__ + `_`_
 1) ___DATA__ (ARGS IN LAMBDAS) ARE A __SINGLE LOWERCASE LETTER___
 
### LAMBDA STRUCTURE:
 0) `LambdaName := \<args>.<returned operation>`
 1) ___IMMUTABLE___
 2) ___CURRY ARGS___
 3) ___RETURN THEIR BODY___
 
### EMBEDDING JS:
0) _SWAP BTWN "__LAMBDA CALCULUS__" & "__JAVASCRIPT__" SCOPES VIA TAGS:_
   * `@JS` = _ALL FOLLOWING CODE IS JAVASCRIPT TO BE LEFT AS IS_
   * `@LC` = _ALL FOLLOWING CODE IS LAMBDA CALC TO BE CONVERTED_
 ```
  @JS
    .. your JS here ..
  @LC
    .. your Lambda Calculus here ..
 ```

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
* `Div  (N1)(N2)` => _N1 / N2_
* `Log  (N1)(N2)` => _log N1 (N2)_

* `Succ (N)` => _Succesor of N,    N+1_
* `Pred (N)` => _Predecessor of N, N-1_

* `Factorial       (N)`      => _N! (w/o Loops, Recursion, or Mutability!!!)_
* `NumericSum      (N)`      => _Sum (0,N)_
* `NumericSumRange (N1)(N2)` => _Sum (N1,N2)_
 
 ----------------------------------------------------------------------------
### LIST (PURELY-FUNCTIONAL!) DATA STRUCTURE FCNS:

#### CONTAINMENT:
* _Lists can contain __ANYTHING:__ including integers, Church Numerals, & __even other Lists!___
 
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
* `Insert (N)(X)(L)` => _Returns List w/ X inserted in L __AFTER__ nth position_
* `Erase  (N)(L)`    => _Returns List w/ L's nth value erased_
* `Push   (X)(L)`    => _Returns List w/ X in front of L_
* `Pop    (L)`       => _Returns List w/o L's Head_
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
* ***Nested-List Access*** => _Any permutation (length 1-4) of 'a' & 'd' btwn 'c' & 'r'!_

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
* *Singleton/HoldArg, Thrush*:     `Th := \af.fa       = C I`
* *Tuple/HoldArgPair, Vireo*:      `V  := \abf.fab     = B C Th = B C (C I)`
