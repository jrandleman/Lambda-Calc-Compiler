// Author: Jordan Randleman - LambdaCalc.hpp 
//   => Library for Lambda Calculus + Custom "ListN" Purely-Fcnal Data Struct!

#ifndef LAMBDA_CALC_HPP_
#define LAMBDA_CALC_HPP_
#include <iostream>

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
 *   => Push (X)(L) => Returns List w/ X in front of L
 *   => Pop  (L)    => Returns List w/o L's Head
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
* LAMBDA CALCULUS IN C++ NAMESPACE
******************************************************************************/

// => PART1: https://www.youtube.com/watch?v=3VQ382QG-y4&feature=youtu.be
// => PART2: https://www.youtube.com/watch?v=pAnLQ9jwN-E
namespace LambdaCalc {

  // Lambdas for visualizing fcnal booleans:
  const auto bshow = [](const auto f){
    std::cout<<std::boolalpha<<f(true)(false)<<std::endl;
  };
  const auto bprint = [](const auto f){
    std::cout<<std::boolalpha<<f(true)(false) << " ";
  };

  // Lambdas for visualizing church numerals (more on this below!):
  const auto nshow = [](const auto f){
    std::cout << f([=](const unsigned long long x){return x+1;})(0) << std::endl;
  };
  const auto nprint = [](const auto f){
    std::cout << f([=](const unsigned long long x){return x+1;})(0) << " ";
  };

  // Lambda to show any data, w/ or w/o '\n'
  const auto show = [](const auto data){std::cout << data << std::endl;};
  const auto print = [](const auto data){std::cout << data;};

  /********************************************************************************
   *  \\   //===)  //^\\  /|    //===)       //|    // (==\\  
   *  /\\  ||     |/===\| ||    ||            ||   //   __//  
   * // \\ \\===) ||   || \===/ \\===)       ==== //   (====/
  ********************************************************************************/

  // Idiot (IDENTITY/ID): I := \a.a
  const auto I = [](const auto a){return a;};

  // Kestrel (FIRST/TRUE/CONST): K := \ab.a
  const auto K = [](const auto a){return [=](const auto b){return a;};};

  // Kite (SECOND/FALSE): KI := \ab.b
  const auto KI = K(I);

  // Mockingbird (SELF-APPLICATION): M := \f.ff
  const auto M = [](const auto f){return f(f);};

  // Cardinal (FLIP): C := \fab.fba
  const auto C = [](const auto f){return [=](const auto a){return [=](const auto b){
    return f(b)(a);
  };};};


  // BOOLEAN OPERATIONS IN LAMBDA CALCULUS:
  // => ALL BOOLEANS ARE IN THE CONTEXT OF MAKING A CHOICE: bool ? true : false;
  // => HENCE "True" selects arg1 & "False" selects arg2
  const auto True = K;
  const auto False = KI;

  // Logical NOT (FLIP 1ST & 2ND ARGS SO 'True' CHOOSES 'False' VALUE): Not := \p.pFT = C
  const auto Not = C;

  // Logical AND: And := \pq.pqF = \pq.pqp
  const auto And = [](const auto p){return [=](const auto q){return p(q)(False);};};

  // Logical OR: Or := \pq.pTq = \pq.ppq = \pq.Mpq
  const auto Or = [](const auto p){return [=](const auto q){return p(True)(q);};};

  // Logical XOR: Xor := \pq.p(Not q)q
  const auto Xor = [](const auto p){return [=](const auto q){return p(Not(q))(q);};};

  // Boolean Equality, Logical XNOR: Beq := \pq.pq(Not q)
  const auto Beq = [](const auto p){return [=](const auto q){return p(q)(Not(q));};};

  /********************************************************************************
   *  \\   //===)  //^\\  /|    //===)       (==\\    // (==\\  
   *  /\\  ||     |/===\| ||    ||            __//   //   __//  
   * // \\ \\===) ||   || \===/ \\===)       (====/ //   (====/
  ********************************************************************************/

  // Bluebird (UNARY COMPOSITION (.)): B := \fga.f(ga)
  const auto B = [](const auto f){return [=](const auto g){return [=](const auto a){
    return f(g(a));
  };};};

  // Blackbird (BINARY COMPOSITION): B1 := \fgab.f(gab) = B B B
  const auto B1 = B(B)(B); // B(B)(B)fgab = B(B(f))gab = B(f)(g(a))b = f(g(a)(b)) = f(gab)

  // Thrush (EXPONENTIATION) (**k): Th := \nk.kn = C(I)
  // (TWICE THRICE) f = TWICE (TWICE (TWICE f)) 
  //                  = (TWICE (TWICE f)) . (TWICE (TWICE f)) 
  //                  = (TWICE f) . (TWICE f) . (TWICE f) . (TWICE f)
  //                  = f . f . f . f . f . f . f . f
  //                  = EIGHTFOLD f
  const auto Th = C(I);

  // Church Numerals (N-Fold Compositions of Fcns) => USED TO COUNT
  const auto Zero = False; // \fa.a  -- Zero Compositions
  const auto Once = I;     // \fa.fa -- Compose Once
  const auto Twice = [](const auto f){return [=](const auto a){return f(f(a));};}; // \fa.f(fa)
  const auto Thrice = [](const auto f){return [=](const auto a){return f(f(f(a)));};}; // \fa.f(f(fa))
  const auto Fourfold = [](const auto f){return [=](const auto a){return f(f(f(f(a))));};}; // \fa.f(f(f(fa)))
  const auto Fivefold = [](const auto f){return [=](const auto a){return f(f(f(f(f(a)))));};}; // \fa.f(f(f(f(fa))))

  // Succesor (+1): Succ := \nf.Bf(nf)
  const auto Succ = [](const auto n){return [=](const auto f){return B(f)(n(f));};};

  // Addition (+k): Add := \nk.k Succ n
  const auto Add = [](const auto n){return [=](const auto k){return k(Succ)(n);};};

  // Multiplication (*k): Mult := \nk.B n k = B
  // THRICE (TWICE f) = THRICE (f . f) 
  //                  = (f . f) . (f . f) . (f . f) 
  //                  = SIXFOLD f
  const auto Mult = B;

  // Exponentiation (**k): Pow := Th
  const auto Pow = Th;

  // Vireo (PAIR DATA STRUCTURE): V := \abf.fab = B C Th
  // V a b f := (B C Th) a b f
  //          = C(Th a) b f      -- Unary composition gobbles up the 1st curry !!!
  //          = C((C I) a) b f
  //          = ((C I) a) f b
  //          = C I a f b        -- Rm parens (syntactically equivalent to currying either way) 
  //          = I f a b
  //          = f a b
  const auto V = B(C)(Th);

  // First  (ACCESS 1ST FCN IN VIREO PAIR p): Fst := \p.p K
  const auto Fst = [](const auto p){return p(K);};

  // Second (ACCESS 2nd FCN IN VIREO PAIR p): Snd := \p.p KI
  const auto Snd = [](const auto p){return p(KI);};

  // Phi (GIVEN VIREO PAIR p) (a,b)->(b,b+1): Phi := \p.V(Snd p)(Succ (Snd p))
  // GIVEN (m,n) RETURNS (n,n+1) => REGARDLESS OF 'm' VALUE!
  // THUS => (0,0) -> (0,1) [1st invocation]
  //      => (0,1) -> (1,2) [2nd invocation]
  //      => (m,n) -> (n,n+1) ["n+1"th invocation]
  const auto Phi = [](const auto p){return V(Snd(p))(Succ(Snd(p)));};

  // Predecessor (-1): Pred := \n.Fst(n Phi (V Zero Zero))
  // HOW: counts from (0,0), storing the current# & next#, until next# = n, where current# is returned
  const auto Pred = [](const auto n){return Fst(n(Phi)(V(Zero)(Zero)));};

  // Subtraction (-k): Sub := \nk.k Pred n
  // => BEWARE OF DOING THIS GENERICALLY IN HASKELL: 
  //    HINDLEY-MILNER TYPE INFERENCE CAN'T HANDLE THE ABSTRACTION
  //    BEYOND SUBTRACTING MORE THAN "Once" FROM ANY OTHER NUMBER
  const auto Sub = [](const auto n){return [=](const auto k){return k(Pred)(n);};};

  // Is0 (==0): Is0 := \n.n(K F)T
  //   => "Constant" False _ONLY_ Applied if n > Zero !
  const auto Is0 = [](const auto n){return n(K(False))(True);};

  // Less Than Or Equal (<=): Leq := \nk.B1 Is0 Sub n k
  const auto Leq = [](const auto n){return [=](const auto k){return B1(Is0)(Sub)(n)(k);};};

  // Equal (==): Eq := \nk.And(Leq n k)(Leq k n)
  const auto Eq = [](const auto n){return [=](const auto k){return And(Leq(n)(k))(Leq(k)(n));};};

  // Greater Than Or Eq (>=): Geq := \nk.B1 Is0 Sub k n
  const auto Geq = [](const auto n){return [=](const auto k){return B1(Is0)(Sub)(k)(n);};};

  // Greater Than (>): Gt := \nk.B1 Not Leq n k
  const auto Gt = [](const auto n){return [=](const auto k){return B1(Not)(Leq)(n)(k);};};

  // Less Than (<): Lt := \nk.B1 Not Geq n k
  const auto Lt = [](const auto n){return [=](const auto k){return B1(Not)(Geq)(n)(k);};};

  /**********************************************************************************
   *  \\   //===)  //^\\  /|    //===)    /|==\ \\ // /|==\\ /|    //==\\ ||^\\ /|==\ 
   *  /\\  ||     |/===\| ||    ||        ||=    )X(  ||==// ||    ||  || ||_// ||=   
   * // \\ \\===) ||   || \===/ \\===)    \|==/ // \\ ||     \===/ \\==// || \\ \|==/
  **********************************************************************************/

  /******************************************************************************
  * "IS-FACTOR" FCNAL BOOLEAN EXPRESSION FOR 2 CHURCH NUMERALS
  ******************************************************************************/

  // Returns Vireo where Fst = counter+1==m?Zero:counter+1, Snd = counter+1==m
  // IsFactorPhi := \mp.V (Eq(B Succ Fst p)m Zero (B Succ Fst p)) (Eq(B Succ Fst p)m)
  const auto IsFactorPhi = [](const auto m){return [=](const auto p){
    return V (Eq(B(Succ)(Fst)(p))(m)(Zero)(B(Succ)(Fst)(p))) (Eq(B(Succ)(Fst)(p))(m));
  };};

  // Returns whether Church Numeral 'm' is a factor Church Numeral 'n'
  // IsFactor := \mn.Is0 m False (Is0 n True (B Is0 Pred m True (Gt m n False (Eq m n True (Snd (n (IsFactorPhi m) (V Zero True)))))))
  const auto IsFactor = [](const auto m){return [=](const auto n){
    return Is0(m) 
            (False)         // can't divide by 0
            (Is0(n)
              (True)        // 0 can be factored by all non-zero #s
              (B(Is0)(Pred)(m)
                (True)      // 1 = factor of all #s
                (Gt(m)(n)   // (m > n) -> not a factor
                  (False)
                  (Eq(m)(n) // #s are factors of themselves
                    (True)
                    (Snd(n(IsFactorPhi(m))(V(Zero)(True))))))));
  };};

  /******************************************************************************
  * EVEN/ODD 'P'REDICATES FOR A CHURCH NUMERAL
  ******************************************************************************/

  // Returns fcnal bool as to whether Church-Numeral is even
  // Evenp := IsFactor Twice
  const auto Evenp = IsFactor(Twice);

  // Returns fcnal bool as to whether Church-Numeral is odd
  // Oddp := B1 Not IsFactor Twice
  const auto Oddp = B1(Not)(IsFactor)(Twice);

  /******************************************************************************
  * CUSTOM FCNAL DATA STRUCTURE => "TRIPLE" !!!
  ******************************************************************************/

  // CUSTOM FCNAL DATA STRUCTURE => "TRIPLE" !!!
  // TRIPLE-PAIR! (Vireo+1) Triple := B1 C V = (B B B)C(B C Th)
  //   => Triple a b c f := (B1 C V) a b c f = C(V a b)c f = (V a b)f c = f a b c
  const auto Triple = B1(C)(V);

  // Fst, Snd, & 'Ternary' GETTERS FOR THE PURELY FCNAL "Triple" Data Structure!
  //   => 'Fst' returns 1st cell in Triple, 'Snd' the 2nd, & 'Trn' the 3rd !!!
  // tFst := \p.p(B1 K K), tSnd := \p.p(B1 K KI), tTrn := \p.p(B1 KI K)
  const auto tFst = [](const auto p){return p(B1(K)(K));};
  const auto tSnd = [](const auto p){return p(B1(K)(KI));};
  const auto tTrn = [](const auto p){return p(B1(KI)(K));}; // 'K' here could also be KI or I

  // Preforms Phi + Applies a given fcn to the old tSnd & tTrn & stores the result in the new tTrn
  //   => Takes fcn as first arg to be able to bind it as a single fcn w/ Church Numerals
  // TriplePhiApply := \fp.Triple(tSnd p)(B Succ tSnd p)(f(tSnd p)(tTrn p))
  const auto TriplePhiApply = [](const auto f){
    return [=](const auto p){return Triple(tSnd(p))(Succ(tSnd(p)))(f(tSnd(p))(tTrn(p)));};
  };

  // MAPPING HELPER LAMBDA USING "Triple" DATA STRUCT !!! 
  //   => apply binary fcn f across Integers [m,n]
  // NumericApplyToRange := \fmn.tTrn(n(TriplePhiApply f)(Triple Zero Once m))
  //   => Enables 'mapping' a fcn across integers w/ a 'Triple'
  const auto NumericApplyToRange = [](const auto f){return [=](const auto m){return [=](const auto n){
    return tTrn(n(TriplePhiApply(f))(Triple(Zero)(Once)(m)));
  };};};

  // Factorial := NumericApplyToRange Mult Once
  const auto Factorial = NumericApplyToRange(Mult)(Once);
  // NumericSum [0,n]: NumericSum := NumericApplyToRange Add Zero
  const auto NumericSum = NumericApplyToRange(Add)(Zero);
  // NumericSum [m,n]: NumericSumRange := \mn.Sub(NumericSum n)(NumericSum m)
  const auto NumericSumRange = [](const auto m){return [=](const auto n){
    return Sub(NumericSum(n))(NumericSum(m));
  };};

  /******************************************************************************
  * GENERIC PURELY FCNAL "LIST" DATA STRUCTURE
  ******************************************************************************/

  // List Abstraction: given n vars + f, apply f to all vars: B(length-2) C (Previous List)
  // Composition Abstraction: (LENGTH == # OF ARGS BEING COMPOSED W/IN 'g') B + B*2*(length-1)

  // Successive Composition of b: Compose b's # of composed args + 1 
  //   => IE: Bluebird = SuccB(I) // B(B)(I) c l = B(I(c))l = B(c)(l) => B B I = B
  //   => IE: Blackbird = SuccB(Bluebird)
  // SuccB := B B
  const auto SuccB = B(B); 

  // Successive List of l using composition b: 
  //   => IE: Tuple = SuccList(I)(Single)
  //   => IE: Triple = SuccList(B)(Tuple)
  // SuccList := \b.(SuccB b) C
  const auto SuccList = [](const auto b){return SuccB(b)(C);};


  // (Thrush) Single := \af.fa
  const auto Single = C(I);

  // (Vireo) Tuple := \abf.fab = B C Single
  const auto Tuple = B(C)(Single);

  // CUSTOM TRIPLE-PAIR! (Vireo+1) Triple := B1 C V = (B B B)C(B C Th)
  //   => Triple a b c f := (B1 C V) a b c f = C(V a b)c f = (V a b)f c = f a b c
  const auto Tripair = B1(C)(Tuple); // What "Triple" above is


  // VIREO PAIR GIVEN CONTAINS COMPOSER IN "Fst", LIST IN "Snd"
  // PhiList := \p.V (SuccB (Fst p)) (SuccList (Fst p) (Snd p))
  const auto PhiList = [](const auto p){return V(SuccB(Fst(p)))(SuccList(Fst(p))(Snd(p)));};

  // Returns Vireo Pair w/ Composer in 'Fst' & List in 'Snd', both handling 'n' args
  // ComposerListPairN := \n.n PhiList (V I Single)
  const auto ComposerListPairN = [](const auto n){return n(PhiList)(V(I)(Single));};


  // Returns Fcnal List Data Struct length n 
  //   => IE: ComposerN(Zero) = I, ComposerN(Once) = B, ComposerN(Twice) = B1, etc
  // ComposerN := B Fst ComposerListPairN
  const auto ComposerN = B(Fst)(ComposerListPairN);

  // Returns Fcnal List Data Struct length n 
  //   => IE: ListN(Zero) = Th, ListN(Once) = Tuple, ListN(Twice) = Triple, etc
  // ListNData := \n.(Is0 n) Th (Snd (ComposerListPairN n))
  const auto ListNData = [](const auto n){return Is0(n)(Th)(Snd(ComposerListPairN(n)));};

  /******************************************************************************
  * "ListN" PURELY FCNAL DATA STRUCT OF LIST FOR 'N' VALUES
  *   + "Length" Method && 2 IS-EMPTY-LIST PREDICATES !!!
  ******************************************************************************/

  // Returns a Tuple (Vireo) w/ list length in Fst & the list itself in Snd
  // n = list length
  // ListN := \n.B ComposerN Succ n (V n) ListNData (Pred n) 
  //   => Add 1 to Composer to also compose (Pred(n)) along w/ list
  const auto ListN = [](const auto n){return B(ComposerN)(Succ)(n)(V(n))(ListNData)(Pred(n));};

  // Length of a List & Contents/Body of a List in: Vireo(Length)(List-Contents)
  const auto Length = Fst; // Length := Fst (PUBLIC)
  const auto Data = Snd;   // Data := Snd   (PRIVATE)

  // Empty & Non-Empty List 'P'redicates
  // Nullp := B Is0 Length, Pairp := B Not Nullp
  const auto Nullp = B(Is0)(Length);
  const auto Pairp = B(Not)(Nullp);

  /******************************************************************************
  * ARBITRARY "ListN" ELT-FCN ACCESSORS ("GETTERS")
  ******************************************************************************/

  // __PRIVATE__: Returns Last Curried Arg of total # (n+1) of curried args
  // GetLast := \n.(Pred n) K KI
  const auto GetLast = [](const auto n){return Pred(n)(K)(KI);};

  // __PRIVATE__: Returns First Curried Arg of total # (n+1) of curried args
  // GetFirst := \n.n K
  const auto GetFirst = [](const auto n){return n(K);};


  // __PUBLIC__: FOR LIST CONSTRUCTS: Returns First fcn in list 'l'
  // Head := \l.B (Data l) GetFirst (Pred (Length l))
  const auto Head = [](const auto l){return B(Data(l))(GetFirst)(Pred(Length(l)));};

  // __PUBLIC__: FOR LIST CONSTRUCTS: Returns Last fcn in list 'l'
  // Last := \l.Eq(Length l)Once (Head l) (B(Data l)GetLast(Pred(Length l)))
  const auto Last = [](const auto l){
    return Eq(Length(l))(Once)
            (Head(l)) 
            (B(Data(l))(GetLast)(Pred(Length(l))));
  };

  // __PUBLIC__: Returns 'nth' Fcn (STARTING AT 1) in list 'l'
  // Nth := \nl.(Data l)((B Is0 Pred n)
  //              (B GetFirst Pred (Length l))
  //              ((ComposerN n)(B1 GetFirst Sub (Length l) n)(B GetLast Pred n)))
  // => Great Heavens it appears I've gotten valid C++ to look like LISP
  const auto Nth = [](const auto nth){return [=](const auto l){
    return Data(l)(B(Is0)(Pred)(nth) 
            (B(GetFirst)(Pred)(Length(l))) 
            (ComposerN(nth) (B1(GetFirst)(Sub)(Length(l))(nth)) (B(GetLast)(Pred)(nth))));
  };};

  /******************************************************************************
  * PUSH/POP FCN TO/FROM FRONT OF LIST
  ******************************************************************************/

  // Returns list w/ 'elt' in front of 'l's list (Push to front)
  // Push := \el.Is0(Length l)(ListN Once e)((Data l)(ListN(B Succ Length l)e))
  const auto Push = [](const auto elt){return [=](const auto l){
    return Is0(Length(l))
            (ListN(Once)(elt))
            (Data(l)(ListN(B(Succ)(Length)(l))(elt)));
  };};


  // Reduces "position" in "Fst" by 1, pushes fcn at "position" in 'l' to 
  //   temporary List in "Snd"
  // PopPhi := \lp.V(B Pred Fst p)(Push(B Nth Fst p l)(Snd p))
  const auto PopPhi = [](const auto l){return [=](const auto p){
    return V (B(Pred)(Fst)(p)) (Push(B(Nth)(Fst)(p)(l))(Snd(p)));
  };};

  // Returns list w/o list l's "Head" (Pops front)
  // => Does so by pushing every fcn from position "Twice" (in 'l') onward 
  //    to a temporary List in "Snd" (the current position is kept in "Fst") 
  //    then returning the temporary List
  // Pop := \l.Is0(B Pred Length l)
  //             (ListN Zero)
  //             (Snd(Sub(Length l)Twice (PopPhi l) (V(B Pred Length l)(ListN Once (Last l)))))
  const auto Pop = [](const auto l){
    return Is0(B(Pred)(Length)(l))
            (ListN(Zero))
            (Snd( Sub(Length(l))(Twice) (PopPhi(l)) (V (B(Pred)(Length)(l)) (ListN(Once)(Last(l))) ) ));
  };

  /******************************************************************************
  * FILTER LIST'S CELLS
  ******************************************************************************/

  // Decrements "position" in "Fst", & Pushes List 'l's cell to temporary list 
  //   in "Snd" IFF cell value adheres to the given Unary "fcnal" Predicate 'f'
  // FilterPhi := \lfp.V(B Pred Fst p)( f(B Nth Fst p l) (Push(B Nth Fst p l)(Snd p)) (Snd p))
  const auto FilterPhi = [](const auto l){return [=](const auto f){return [=](const auto p){
    return V (B(Pred)(Fst)(p)) (f(B(Nth)(Fst)(p)(l)) (Push(B(Nth)(Fst)(p)(l))(Snd(p))) (Snd(p)));
  };};};

  // Returns list w/ cells from List 'l' Filtered thru Unary "fcnal" Predicate 'f'
  // Filter := \fl.Snd (Length l (FilterPhi l f) (V (Length l) (ListN Zero)))
  const auto Filter = [](const auto f){return [=](const auto l){
    return Snd( Length(l) (FilterPhi(l)(f)) (V (Length(l)) (ListN(Zero))) );
  };};

  /******************************************************************************
  * MAP FCN ACROSS LIST
  ******************************************************************************/

  // Decrements current position & Pushes mapped elt to temporary List in "Snd"
  // MapPhi := \lfp.V(B Pred Fst p)(Push(f(B Nth Fst p l))(Snd p))
  const auto MapPhi = [](const auto l){return [=](const auto f){return [=](const auto p){
    return V (B(Pred)(Fst)(p)) (Push(f(B(Nth)(Fst)(p)(l)))(Snd(p)));
  };};};

  // Returns list of each fcn in List 'l' after passing thru fcn 'f'
  // Map := \fl.Is0(Length l)l(Snd(B Pred Length l(MapPhi l f)(V (B Pred Length l)(ListN Once(B f Last l)))))
  const auto Map = [](const auto f){return [=](const auto l){
    return Is0(Length(l))
            (l)
            (Snd( B(Pred)(Length)(l) (MapPhi(l)(f)) (V (B(Pred)(Length)(l)) (ListN(Once)(B(f)(Last)(l)))) ));
  };};

  /******************************************************************************
  * IMPURE MAP (MAPS "VOID" FCN ACROSS ELTS => SIDE EFFECTS)
  * HELPS APPLY "PRINTER" FCNS TO EACH ELT IN LIST
  ******************************************************************************/

  // -:- IMPURE -:- Applies 'f' to 'n'th fcn in list 'l', PERFORMING A SIDE EFFECT
  const auto IMPURE_VoidMapPhi = [](const auto l){return [=](const auto f){return [=](const auto n){
    f(Nth(n)(l)); // SIDE EFFECT MAY OCCUR HERE
    return Succ(n);
  };};};

  // -:- IMPURE -:- Maps fcn 'f' over list 'l' (generally for passing a printing fcn)
  // => !!! FCN 'f' SHALL BE VOID !!!
  // => !!! "VoidMap" IS VOID !!!
  // => "IMPURE" b/c fcn given must be performing side effects: likely I/O
  //    => Needed However to easy enable printing all fcns in a List.
  const auto VoidMap = [](const auto f){return [=](const auto l){
    Length(l)(IMPURE_VoidMapPhi(l)(f))(Once);
  };};

  /******************************************************************************
  * REVERSE A LIST
  ******************************************************************************/

  // Returns Vireo w/ "Fst"'s "position" Increased & having pushed fcn at 
  //   "position" in "l" to the temporary List in "Snd"
  // ReversePhi := \lp.V(B Succ Fst p)(Push(B Nth Fst p l)(Snd p))
  const auto ReversePhi = [](const auto l){return [=](const auto p){
    return V (B(Succ)(Fst)(p)) (Push(B(Nth)(Fst)(p)(l))(Snd(p)));
  };};

  // Returns list of 'l' in Reverse
  // => Does so by pushing every elt - IN ORDER - from 'l' to the front of a 
  //    temporary List in "Snd" (current position kept in "Fst") then returning
  //    the temporary list
  // Reverse := \l.Is0(B Pred Length l)
  //                l
  //                (Snd(B Pred Length l(ReversePhi l)(V Twice(ListN Once(Head l)))))
  const auto Reverse = [](const auto l){
    return Is0(B(Pred)(Length)(l)) 
            (l)
            (Snd( B(Pred)(Length)(l) (ReversePhi(l)) (V(Twice)(ListN(Once)(Head(l)))) ));
  };

  /******************************************************************************
  * BACKWARD LIST FCN APPLICATION
  ******************************************************************************/

  // Flips Args in a Fcn
  // FlipArgs := C
  const auto FlipArgs = C;

  // Applies fcn 'f' to a list in reverse, _RETURNING A LIST_
  //   => From which may derive push_back, pop_back, etc
  // Backward := \f.B Reverse(B f Reverse)
  const auto Backward = [](const auto f){return B(Reverse)(B(f)(Reverse));};

  // Applies fcn 'f' to a list in reverse, _RETURNING AN ATOM (IE Non-List Fcn)_
  // BackwardAtomic := \f.B f Reverse
  const auto BackwardAtomic = [](const auto f){return B(f)(Reverse);};

  /******************************************************************************
  * LIST ACCUMULATOR
  ******************************************************************************/

  // Applies Phi & Folds Nth cell in List 'l' (Nth's n coming from (tSnd p))
  //   over the prior value in (tTrn p)
  // FoldlPhi := \flp.Triple(tSnd p)(B Succ tSnd p)(f(Nth(tSnd p)l)(tTrn p))
  const auto FoldlPhi = [](const auto f){return [=](const auto l){return [=](const auto p){
    return Triple(tSnd(p))(Succ(tSnd(p)))(f(Nth(tSnd(p))(l))(tTrn(p)));
  };};};

  // Fold cells in List 'l' on top of one another w/ BINARY fcn 'f',
  //   starting from "elt" on (Head l) (from 'l'eft to right)
  // Foldl := \fel.Is0(Length l)Zero(tTrn((Length l)(FoldlPhi f l)(Triple Zero Once e)))
  const auto Foldl = [](const auto f){return [=](const auto elt){return [=](const auto l){
    return Is0(Length(l))
            (Zero)
            (tTrn((Length(l))(FoldlPhi(f)(l))(Triple(Zero)(Once)(elt)))); // (Once) b/c Nth starts @ 1
  };};};

  // Fold cells in List 'l' on top of one another w/ BINARY fcn 'f',
  //   starting from "elt" on (Last l) (from 'r'ight to left)
  // Foldr := \fel.Is0(Length l)Zero(BackwardAtomic (Foldl f e) l)
  const auto Foldr = [](const auto f){return [=](const auto elt){return [=](const auto l){
    return Is0(Length(l))
            (Zero)
            (BackwardAtomic (Foldl(f)(elt)) (l));
  };};};

  /******************************************************************************
  * MAX/MIN OF LIST`
  ******************************************************************************/

  // Returns Greatest Value in List
  //   => Accumuates Greater Value, starting relative to Zero
  // Max := Foldl(\ab.Gt a b a b)Zero
  const auto Max = Foldl([](const auto a){return [=](const auto b){
    return Gt(a)(b)(a)(b);
  };})(Zero);

  // Returns Smallest Value in List
  //   => Accumuates Smaller Value, starting relative to 1st Value in List
  // Min := \l.Foldl(\ab.Lt a b a b)(Head l)l
  const auto Min = [](const auto l){return Foldl([](const auto a){return [=](const auto b){
      return Lt(a)(b)(a)(b);
    };})(Head(l))(l);
  };

  /******************************************************************************
  * LISP-STYLE LIST TRAVERSAL
  ******************************************************************************/

  // Car := Head, Cdr := Pop
  // Cadr := B Car Cdr, Caddr := B Car(Twice Cdr), Cadddr := B Car(Thrice Cdr)
  const auto car = Head;
  const auto cdr = Pop;
  const auto cadr = B(car)(cdr);
  const auto caddr = B(car)(Twice(cdr));
  const auto cadddr = B(car)(Thrice(cdr));

  /******************************************************************************
  * CHURCHILL NUMERALS A LA HEX (ox0-ox5 == Zero-Fivefold)
  ******************************************************************************/
  
  const auto ox0 = False; // \fa.a  -- Zero Compositions
  const auto ox1 = I;     // \fa.fa -- Compose Once
  const auto ox2 = [](const auto f){return [=](const auto a){return f(f(a));};}; // \fa.f(fa)
  const auto ox3 = [](const auto f){return [=](const auto a){return f(f(f(a)));};}; // \fa.f(f(fa))
  const auto ox4 = [](const auto f){return [=](const auto a){return f(f(f(f(a))));};}; // \fa.f(f(f(fa)))
  const auto ox5 = [](const auto f){return [=](const auto a){return f(f(f(f(f(a)))));};};
  const auto ox6 = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(a))))));};};
  const auto ox7 = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(f(a)))))));};};
  const auto ox8 = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(f(f(a))))))));};};
  const auto ox9 = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(f(f(f(a)))))))));};};
  const auto oxa = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(f(f(f(f(a))))))))));};};
  const auto oxb = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(f(f(f(f(f(a)))))))))));};};
  const auto oxc = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(f(f(f(f(f(f(a))))))))))));};};
  const auto oxd = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(f(f(f(f(f(f(f(a)))))))))))));};};
  const auto oxe = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(f(f(f(f(f(f(f(f(a))))))))))))));};};
  const auto oxf = [](const auto f){return [=](const auto a){return f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(a)))))))))))))));};};

}; // End of LambdaCalc Namespace
#endif
