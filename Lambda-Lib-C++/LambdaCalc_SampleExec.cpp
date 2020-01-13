// Author: Jordan Randleman -- LambdaCalc_SampleExec.cpp
//   => Demo File to Illustrate LambdaCalc.hpp's Capabilities

#include <iostream>
#include "LambdaCalc.hpp"

/**
 * -:- NAMESPACE LambdaCalc LAMBDAS -:-
 *     => ALL DATA IS IMMUTABLE (CONST)
 *     => ALL LAMBDAS ARE CURRIED ( IE Add(ox1, ox2) => Add(ox1)(ox2) )
 *     => CAPTURE SCOPE BY _VALUE_ (using [=]) FOR INNER - CURRIED! - LAMBDAS
 *
 * !!!!! GENERATE A LIST OF ARBITRARY LENGTH W/O MEMORY ALLOCATION (Push) !!!!!
 *
 * NOTATION:
 *  => let N = Church Numeral, B = Fcnal Bool, F1 = Unary Fcn, F2 = Binary Fcn,
 *         F = arbitrary fcn, X = arbitrary data, 
 *         L = Fcnal List Data Structure (See More Below)
 *
 * ----------------------------------------------------------------------------
 * - VISUALIZATION:
 * ----------------------------------------------------------------------------
 *
 * show(X)   => print arbitrary data x to screen + newline
 * print(X)  => print arbitrary data x to screen
 *
 * bshow(B)  => print fcnal boolean as boolean boolean + newline
 * bprint(B) => print fcnal boolean as boolean boolean
 *
 * nshow(N)  => print church numeral as unsigned long long + newline
 * nprint(N) => print church numeral as unsigned long long
 *
 * ----------------------------------------------------------------------------
 * - FCNAL BOOLEANS:
 * ----------------------------------------------------------------------------
 *
 * EXPLANATION:
 *   Fcnal Booleans are Binary Fcns, acting like C++'s Ternary '?:' operator: 
 *     => Fcnal 'True' chooses arg1, 'False' chooses arg2
 * 
 * BOOLEANS:
 *   => True
 *   => False
 *
 * BOOLEAN OPERATIONS:
 *   => Not (B)
 *   => And (B1)(B2)
 *   => Or  (B1)(B2)
 *   => Xor (B1)(B2)
 *   => Beq (B1)(B2) => 'B'oolean 'eq'uality, ie xnor
 * 
 * ----------------------------------------------------------------------------
 * - CHURCH-NUMERAL NUMERIC FCNS:
 * ----------------------------------------------------------------------------
 *
 * EXPLANATION:
 *   N-Fold Compositions of a Fcn (!!! ALL >= Zero Integers !!!):
 *    => IE Zero = a, Once = f(a), Twice = f(f(a)),  Thrice = f(f(f(a))), etc
 *
 * NUMERALS:
 *   => Zero, Once, Twice, Thrice, Fourfold, Fivefold
 *   => ox0,ox1,ox2,ox3,ox4,ox5,ox6,ox7,ox8,ox9,oxa,oxb,oxc,oxd,oxe,oxf
 *
 * COMPARATIVE BOOLEANS:
 *   => Is0 (N) => Equal to 'Zero'
 *
 *   => Eq  (N1)(N2) => Equal-to
 *   => Lt  (N1)(N2) => Less Than
 *   => Gt  (N1)(N2) => Greater Than
 *   => Leq (N1)(N2) => Less Than Or Equal-to
 *   => Geq (N1)(N2) => Greater Than Or Equal-to
 * 
 *   => IsFactor (N1)(N2) => N1 is a factor of N2
 *   => Evenp    (N)      => N is even
 *   => Oddp     (N)      => N is odd
 * 
 * ARITHMETIC:
 *   => Add  (N1)(N2) => N1 + N2
 *   => Sub  (N1)(N2) => N1 - N2
 *   => Mult (N1)(N2) => N1 * N2
 *   => Pow  (N1)(N2) => N1 ** N2
 *   => Div  (N1)(N2) => N1 / N2
 *   => Log  (N1)(N2) => log N1 (N2)
 *
 *   => Succ (N) => Succesor of N,    N+1
 *   => Pred (N) => Predecessor of N, N-1
 *
 *   => Factorial       (N)      => N! (w/o Loops, Recursion, or Mutability!!!)
 *   => NumericSum      (N)      => Sum (0,N)
 *   => NumericSumRange (N1)(N2) => Sum (N1,N2)
 * 
 * ----------------------------------------------------------------------------
 * - PURELY-FCNAL LIST DATA-STRUCTURE FCNS:
 * ----------------------------------------------------------------------------
 * 
 * CONSTRUCTION (Given List Size):
 *   => ListN(N) (N1) (N2) (N3) (NN) => Returns List size N of the trailing elts
 *
 * BASIC ANALYSIS:
 *   => Length (L) => Returns length of L
 *   => Nullp  (L) => "Null" 'p'redicate: List is EMPTY
 *   => Pairp  (L) => "Pair" 'p'redicate: List is __NOT__ EMPTY
 *
 * GETTERS:
 *   => Head (L)    => Return L's 1st cell value
 *   => Last (L)    => Return L's last cell value
 *   => Nth  (N)(L) => Returns L's 'N'th elt (starting from 'ox1')
 * 
 * SETTERS: 
 *   => Insert (N)(X)(L) => Returns List w/ X inserted in L AFTER nth position
 *   => Erase  (N)(L)    => Returns List w/ L's nth value erased
 *   => Push   (X)(L)    => Returns List w/ X in front of L
 *   => Pop    (L)       => Returns List w/o L's Head
 *     => NOTE: "_back" versions may be self-implemented via "Backward" fcn (More Below)
 * 
 * FILTER/MAP/VOIDMAP:
 *   => Filter  (F1)(L) => Returns List having filtered out elts from L __NOT__ passing F1
 *   => Map     (F1)(L) => Returns List having mapped F1 across all of L's elts
 *   => VoidMap (F1)(L) => Returns Void & F1 Must be void => Applie Fcn to each elt in L
 *                         (useful for passing a "printer" fcn (ie nshow) to print each elt)
 *
 * REVERSED LIST & FCN APPLICATION:
 *   => Reverse        (L)     => Returns List of L in reverse
 *   => FlipArgs       (F2)    => Flips args for a Binary Fcn
 *   => Backward       (F1)(L) => Returns List having applied F on Reverse(L)
 *   => BackwardAtomic (F1)(L) => Returns Atom (ie Non-List Fcn, such as a
 *                                Church Numeral) having applied F on Reverse(L)
 * ACCUMULATORS:
 *   => Foldl (F2)(X)(L) => Applies F2 on L from 'l'eft to right, 
 *                          starting w/ 'X' & Head(L)
 *   => Foldr (F2)(X)(L) => Applies F2 on L from 'r'ight to left, 
 *                          starting w/ 'X' & Last(L)
 * MAX/MIN:
 *   => Max (L) => Returns Greatest value in List
 *   => Min (L) => Returns Smallest value in List
 *
 * LISP-STYLE ACCESS:
 *   => car    (L) => Returns Current Cell Value ( IE Head(L) )
 *   => cdr    (L) => Returns Next Cell ( IE Pop(L) )
 *   => cadr   (L) => Head(Pop(L))
 *   => caddr  (L) => Head(Pop(Pop(L)))
 *   => cadddr (L) => Head(Pop(Pop(Pop(L))))
 *   => ANY combo of 1-4 'a's & 'd's btwn 'c' & 'r' for nested list access!
 *
 * ----------------------------------------------------------------------------
 * - IF YOU'VE GOTTEN THIS FAR ...
 * ----------------------------------------------------------------------------
 *
 * You may genuinely enjoy the 2 JS Lambda Calculus videos below, found at: 
 *   => Part 1: https://www.youtube.com/watch?v=3VQ382QG-y4&feature=youtu.be
 *   => Part 2: https://www.youtube.com/watch?v=pAnLQ9jwN-E
 * 
 * In Summary:
 *   => Identity/Once,     Idiot:      I := \a.a
 *   => First/True/Const,  Kestrel:    K := \ab.a
 *   => Flip/LogicalNot,   Cardinal:   C := \fab.fba
 *   => Unary Compose,     Bluebird:   B := \fga.f(ga)
 *
 *   => Self-Replication,  Mockingbird M := \f.f(f) => IMPOSSIBLE IN HASKELL (Infinite Data Struct)
 *
 *   => Second/False/Zero, Kite:       KI := \ab.b        = K I = C K
 *   => Binary Compose,    Blackbird:  B1 := \fgab.f(gab) = B B B
 *   => Hold An Arg,       Thrush:     Th := \af.fa       = C I
 *   => Hold Arg Pair,     Vireo:      V  := \abf.fab     = B C Th = B C (C I)
 */

/******************************************************************************
* CURRIED FUNCTIONS & LAMBDA CALCULUS EXECUTION C++
******************************************************************************/

int main() {
  using namespace LambdaCalc;


  show("\nUsing Fcnal Booleans:");

  print("  => Not(True):         ");
  bshow(Not(True));
  print("  => Not(False):        ");
  bshow(Not(False));
  print("  => And(True)(False):  ");
  bshow(And(True)(False));
  print("  => And(True)(True):   ");
  bshow(And(True)(True));
  print("  => And(False)(False): ");
  bshow(And(False)(False));
  print("  => Or(True)(False):   ");
  bshow(Or(True)(False));
  print("  => Or(False)(False):  ");
  bshow(Or(False)(False));
  print("  => Or(True)(True):    ");
  bshow(Or(True)(True));
  print("  => Xor(False)(True):  ");
  bshow(Xor(False)(True));
  print("  => Xor(True)(True):   ");
  bshow(Xor(True)(True));
  print("  => Xor(False)(False): ");
  bshow(Xor(False)(False));
  print("  => Beq(True)(False):  ");
  bshow(Beq(True)(False));
  print("  => Beq(True)(True):   ");
  bshow(Beq(True)(True));
  print("  => Beq(False)(False): ");
  bshow(Beq(False)(False));



  show("\n\n\nUsing Church Numerals (0-15 shown as Hex w/ 'o' prefix):");
  print("  => Is0(ox5): ");
  bshow(Is0(ox5));
  print("  => Is0(ox0): ");
  bshow(Is0(ox0));

  show("");
  print("  => Eq(ox2)(ox8):  ");
  bshow(Eq(ox2)(ox8));
  print("  => Eq(ox2)(ox2):  ");
  bshow(Eq(ox2)(ox2));
  print("  => Lt(ox2)(ox8):  ");
  bshow(Lt(ox2)(ox8));
  print("  => Lt(ox8)(ox2):  ");
  bshow(Lt(ox8)(ox2));
  print("  => Lt(ox8)(ox8):  ");
  bshow(Lt(ox8)(ox8));
  print("  => Gt(ox2)(ox8):  ");
  bshow(Gt(ox2)(ox8));
  print("  => Gt(ox8)(ox2):  ");
  bshow(Gt(ox8)(ox2));
  print("  => Gt(ox8)(ox8):  ");
  bshow(Gt(ox8)(ox8));
  print("  => Leq(ox2)(ox8): ");
  bshow(Leq(ox2)(ox8));
  print("  => Leq(ox8)(ox2): ");
  bshow(Leq(ox8)(ox2));
  print("  => Leq(ox8)(ox8): ");
  bshow(Leq(ox8)(ox8));
  print("  => Geq(ox2)(ox8): ");
  bshow(Geq(ox2)(ox8));
  print("  => Geq(ox8)(ox2): ");
  bshow(Geq(ox8)(ox2));
  print("  => Geq(ox8)(ox8): ");
  bshow(Geq(ox8)(ox8));

  show("");
  print("  => IsFactor(ox3)(oxc): ");
  bshow(IsFactor(ox2)(ox4));
  print("  => IsFactor(ox3)(oxd): ");
  bshow(IsFactor(ox2)(ox7));
  print("  => Evenp(ox6): ");
  bshow(Evenp(ox6));
  print("  => Evenp(ox9): ");
  bshow(Evenp(ox9));
  print("  => Oddp(ox6):  ");
  bshow(Oddp(ox6));
  print("  => Oddp(ox9):  ");
  bshow(Oddp(ox9));

  show("");
  print("  => Add(oxf)(oxa):            ");
  nshow(Add(oxf)(oxa));
  print("  => Sub(oxb)(ox6):            ");
  nshow(Sub(oxb)(ox6));
  print("  => Mult(ox3)(ox7):           ");
  nshow(Mult(ox3)(ox7));
  print("  => Pow(ox2)(ox5):            ");
  nshow(Pow(ox2)(ox5));
  print("  => Div(Mult(ox2)(oxa))(ox4): ");
  nshow(Div(Mult(ox2)(oxa))(ox4));
  print("  => Log(ox2)(ox8):            ");
  nshow(Log(ox2)(ox8));

  show("");
  print("  => Succ(ox8): ");
  nshow(Succ(ox8));
  print("  => Pred(ox8): ");
  nshow(Pred(ox8));

  show("");
  print("  => Factorial(ox5):  ");
  nshow(Factorial(ox5));
  print("  => NumericSum(oxa): ");
  nshow(NumericSum(oxa));
  print("  => NumericSumRange(ox5)(oxa): ");
  nshow(NumericSumRange(ox5)(oxa));



  show("\n\n\nUsing The Purely-Fcnal \"ListN\" Data Structure:");

  show("  => We have defined 2 lists:");
  show("     (1) List of 5 Church Numerals:");
  show("         List1 = ListN(ox5) (ox9) (ox4) (ox7) (ox3) (oxa);");
  const auto List1 = ListN(ox5) (ox9) (ox4) (ox7) (ox3) (oxa);
  show("     (2) Empty List:");
  show("         List2 = ListN(ox0);");
  const auto List2 = ListN(ox0);

  show("\nBASIC ANALYSIS:");
  print("  => Length(List1): ");
  nshow(Length(List1));
  print("  => Length(List2): ");
  nshow(Length(List2));

  show("  => Whether list IS or IS NOT empty:");
  print("     - Nullp(List1): ");
  bshow(Nullp(List1));
  print("     - Nullp(List2): ");
  bshow(Nullp(List2));
  print("     - Pairp(List1): ");
  bshow(Pairp(List1));
  print("     - Pairp(List2): ");
  bshow(Pairp(List2));

  show("\nGETTERS:");
  print("  => Head(List1):     ");
  nshow(Head(List1));
  print("  => Last(List1):     ");
  nshow(Last(List1));
  print("  => Nth(ox1)(List1): ");
  nshow(Nth(ox1)(List1));
  print("  => Nth(ox2)(List1): ");
  nshow(Nth(ox2)(List1));
  print("  => Nth(ox3)(List1): ");
  nshow(Nth(ox3)(List1));
  print("  => Nth(ox4)(List1): ");
  nshow(Nth(ox4)(List1));
  print("  => Nth(ox5)(List1): ");
  nshow(Nth(ox5)(List1));

  show("\nSETTERS:");
  print("  => Length(Push(oxd)(List1)):      ");
  nshow(Length(Push(oxd)(List1)));
  print("  => Head(Push(oxd)(List1)):        ");
  nshow(Head(Push(oxd)(List1)));
  print("  => Length(Pop(List1)):            ");
  nshow(Length(Pop(List1)));
  print("  => Head(Pop(List1)):              ");
  nshow(Head(Pop(List1)));
  print("  => Length(Push(oxf)(List2)):      ");
  nshow(Length(Push(oxf)(List2)));
  print("  => Head(Push(oxf)(List2)):        ");
  nshow(Head(Push(oxf)(List2)));
  print("  => Length(Pop(Push(oxf)(List2))): ");
  nshow(Length(Pop(Push(oxf)(List2))));
  print("  => Erase(ox3)(List1)       = ");
  VoidMap(nprint)(Erase(ox3)(List1));
  print("\n  => Insert(ox3)(oxc)(List1) = ");
  VoidMap(nprint)(Insert(ox3)(oxc)(List1));
  show("");

  show("\nFILTER/MAP/VOIDMAP:");
  show("  => We have defined more 2 lists:");
  show("     (1) List of odd Church Numerals from List1:");
  show("         OnlyOdds = Filter(Oddp)(List1);");
  const auto OnlyOdds = Filter(Oddp)(List1);
  show("     (2) List of 2 raised to each value in List1:");
  show("         PowersOf2 = Map(Pow(ox2))(List1);\n");
  const auto PowersOf2 = Map(Pow(ox2))(List1);
  show("  => Using \"VoidMap\" to map a Void printer fcn across these Lists:");
  show("     (*) NOTE: \"nprint()\" = void lambda to print Church Numerals as ints!");
  print("     (1) VoidMap(nprint)(OnlyOdds)  = ");
  VoidMap(nprint)(OnlyOdds);
  show("");
  print("     (2) VoidMap(nprint)(PowersOf2) = ");
  VoidMap(nprint)(PowersOf2);

  show("\n\nREVERSED LIST & REVERSED FCN APPLICATION:");
  print("  => List1          = ");
  VoidMap(nprint)(List1);
  print("\n  => Reverse(List1) = ");
  VoidMap(nprint)(Reverse(List1));

  print("\n  => Pow(ox2)(ox3)           = ");
  nshow(Pow(ox2)(ox3));
  print("  => FlipArgs(Pow)(ox2)(ox3) = ");
  nshow(FlipArgs(Pow)(ox2)(ox3));

  print("  => Push(oxf)(List1)           = ");
  VoidMap(nprint)(Push(oxf)(List1));
  print("\n  => Backward(Push(oxf))(List1) = ");
  VoidMap(nprint)(Backward(Push(oxf))(List1));
  show("\n  => We have defined 1 more List: List3 = ListN(ox2) (ox2)(ox3);");
  const auto List3 = ListN(ox2) (ox2)(ox3);
  print("     -> Foldl(Pow)(ox1)(List3)                 = ");
  nprint(Foldl(Pow)(ox1)(List3));
  print("\n     -> BackwardAtomic(Foldl(Pow)(ox1))(List3) = ");
  nshow(BackwardAtomic(Foldl(Pow)(ox1))(List3));

  show("\nACCUMULATORS:");
  show("  => Both Accumulators have already been shown, 1 more subtly so:");
  print("     -> Foldl(Pow)(ox1)(List3) = ");
  nshow(Foldl(Pow)(ox1)(List3));
  print("     -> Foldr(Pow)(ox1)(List3) = ");
  nprint(Foldr(Pow)(ox1)(List3));
  show(" // \"Foldr\" = \"BackwardAtomic\" . \"Foldl\"!");

  show("\nMAX/MIN:");
  print("  => Max(List1) = ");
  nshow(Max(List1));
  print("  => Min(List1) = ");
  nshow(Min(List1));

  show("\nLISP-STYLE ACCESS:");
  print("  => List1         = ");
  VoidMap(nprint)(List1);
  print("\n  => car(List1)    = ");
  nshow(car(List1));
  print("  => cdr(List1)    = ");
  VoidMap(nprint)(cdr(List1));
  print("\n  => cadr(List1)   = ");
  nshow(cadr(List1));
  print("  => caddr(List1)  = ");
  nshow(caddr(List1));
  print("  => cadddr(List1) = ");
  nshow(cadddr(List1));

  show("\nLISTS OF LISTS:");
  const auto SuperList1 = ListN(ox3) (ListN(ox2) (ox4) (ox5)) (ListN(ox3) (oxa) (ox2) (ox3)) (ListN(ox1) (ox8));
  show("  => We have defined a list of 3 lists:");
  show("     (*) // SuperList1 = [ [4, 5], [10, 2, 3], [8] ]");
  show("     (0) SuperList1 = ListN(ox3) (ListN(ox2) (ox4) (ox5)) (ListN(ox3) (oxa) (ox2) (ox3)) (ListN(ox1) (ox8));\n");
  print("  => Head(Head(SuperList1))         = ");
  nshow(Head(Head(SuperList1)));
  print("  => Last(Last(SuperList1))         = ");
  nshow(Last(Last(SuperList1)));
  print("  => Nth(ox1)(Nth(ox2)(SuperList1)) = ");
  nshow(Nth(ox1)(Nth(ox2)(SuperList1)));
  show("  => Using LISP Notation:");
  print("     -> caar(SuperList1)   = ");
  nshow(caar(SuperList1));
  print("     -> caaddr(SuperList1) = ");
  nshow(caaddr(SuperList1));
  print("     -> caadr(SuperList1)  = ");
  nshow(caadr(SuperList1));

  show("\nLIST OF MULTIPLE-TYPED ELTS:");
  show("  => We have defined a list w/ a float, String, & Church Numeral:");
  show("     (0) multi_type_list = ListN(ox3) (3.14159) (\"Talk about dynamic!\") (oxd);");
  const auto multi_type_list = ListN(ox3) (3.14159) ("Talk about dynamic!") (oxd);
  print("\n  => car(multi_type_list)   = ");
  show(car(multi_type_list));
  print("  => cadr(multi_type_list)  = ");
  show(cadr(multi_type_list));
  print("  => caddr(multi_type_list) = ");
  nshow(caddr(multi_type_list));



  show("\nBye!\n");

  return 0;
}
