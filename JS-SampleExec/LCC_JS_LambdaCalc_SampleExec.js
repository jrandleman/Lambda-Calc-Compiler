
// JS_LambdaCalc_SampleExec.js

// Show any data
const show = console.log;
const print = a => process.stdout.write(a);

// Lambdas for visualizing fcnal booleans
const bshow = f => show(f(true)(false));
const bprint = f => print((f(true)(false)).toString());

// Lambdas for visualizing church numerals
const nshow = f => show((f(x => x + 1))(0));
const nprint = f => print(`${(f(x => x + 1))(0)} `);

const I = (a)=>(a);
const K = (a)=>(b)=>(a);
const KI = (K)(I);
const C = (f)=>(a)=>(b)=>(f)(b)(a);
const True = (K);
const False = (KI);
const Not = (C);
const And = (p)=>(q)=>(p)(q)(p);
const Or = (p)=>(q)=>(p)(p)(q);
const Xor = (p)=>(q)=>(p)((Not)(q))(q);
const Beq = (p)=>(q)=>(p)(q)((Not)(q));
const B = (f)=>(g)=>(a)=>(f)((g)(a));
const B1 = (B)(B)(B);
const Th = (C)(I);
const Zero = (False);
const Once = (I);
const Twice = (f)=>(a)=>(f)((f)(a));
const Thrice = (f)=>(a)=>(f)((f)((f)(a)));
const Fourfold = (f)=>(a)=>(f)((f)((f)((f)(a))));
const Fivefold = (f)=>(a)=>(f)((f)((f)((f)((f)(a)))));
const Ox0 = (f)=>(a)=>(a);
const Ox1 = (f)=>(a)=>(f)((Ox0)(f)(a));
const Ox2 = (f)=>(a)=>(f)((Ox1)(f)(a));
const Ox3 = (f)=>(a)=>(f)((Ox2)(f)(a));
const Ox4 = (f)=>(a)=>(f)((Ox3)(f)(a));
const Ox5 = (f)=>(a)=>(f)((Ox4)(f)(a));
const Ox6 = (f)=>(a)=>(f)((Ox5)(f)(a));
const Ox7 = (f)=>(a)=>(f)((Ox6)(f)(a));
const Ox8 = (f)=>(a)=>(f)((Ox7)(f)(a));
const Ox9 = (f)=>(a)=>(f)((Ox8)(f)(a));
const Oxa = (f)=>(a)=>(f)((Ox9)(f)(a));
const Oxb = (f)=>(a)=>(f)((Oxa)(f)(a));
const Oxc = (f)=>(a)=>(f)((Oxb)(f)(a));
const Oxd = (f)=>(a)=>(f)((Oxc)(f)(a));
const Oxe = (f)=>(a)=>(f)((Oxd)(f)(a));
const Oxf = (f)=>(a)=>(f)((Oxe)(f)(a));
const Succ = (n)=>(f)=>(B)(f)((n)(f));
const Add = (n)=>(k)=>(k)((Succ))(n);
const Mult = (B);
const Pow = (Th);
const V = (B)(C)(Th);
const Fst = (p)=>(p)(K);
const Snd = (p)=>(p)(KI);
const Phi = (p)=>(V)((Snd)(p))((B)(Succ)(Snd)(p));
const Pred = (n)=>(Fst)((n)(Phi)((V)(Zero)(Zero)));
const Sub = (n)=>(k)=>(k)(Pred)(n);
const Is0 = (n)=>(n)((K)(False))(True);
const Leq = (n)=>(k)=>(B1)(Is0)(Sub)(n)(k);
const Eq = (n)=>(k)=>(And)((Leq)(n)(k))((Leq)(k)(n));
const Geq = (n)=>(k)=>(B1)(Is0)(Sub)(k)(n);
const Gt = (n)=>(k)=>(Not)((Leq)(n)(k));
const Lt = (n)=>(k)=>(Not)((Geq)(n)(k));
const DivPhi = (a)=>(b)=>(p)=>(Geq)((Snd)(p))(a)(p)((V)((B)(Succ)(Fst)(p))((Add)((Snd)(p))(b)));
const DivIter = (a)=>(b)=>(Or)((Lt)(a)(b))((Is0)(b))(Zero)((Fst)((a)((DivPhi)(a)(b))((V)(Zero)(Zero))));
const DivRes = (a)=>(b)=>(n)=>(Gt)((Mult)(b)(n))(a)((Pred)(n))(n);
const Div = (a)=>(b)=>(DivRes)(a)(b)((DivIter)(a)(b));
const LogPhi = (a)=>(b)=>(p)=>(Geq)((Snd)(p))(a)(p)((V)((B)(Succ)(Fst)(p))((Mult)((Snd)(p))(b)));
const LogIter = (a)=>(b)=>(Or)((Lt)(a)(b))((Is0)(b))(Zero)((Fst)((a)((LogPhi)(a)(b))((V)(Zero)(Once))));
const LogRes = (a)=>(b)=>(n)=>(Gt)((Pow)(b)(n))(a)((Pred)(n))(n);
const Log = (a)=>(b)=>(Is0)((Pred)(a))(a)((LogRes)(b)(a)((LogIter)(b)(a)));
const IsFactorPhi = (m)=>(p)=>(V)((Eq)((B)(Succ)(Fst)(p))(m)(Zero)((B)(Succ)(Fst)(p)))((Eq)((B)(Succ)(Fst)(p))(m));
const IsFactor = (m)=>(n)=>(Is0)(m)(False)((Is0)(n)(True)((B)(Is0)(Pred)(m)(True)((Gt)(m)(n)(False)((Eq)(m)(n)(True)((Snd)((n)((IsFactorPhi)(m))((V)(Zero)(True))))))));
const Evenp = (IsFactor)(Twice);
const Oddp = (B1)(Not)(IsFactor)(Twice);
const NumericApplyPhi = (f)=>(p)=>(V)((B)(Succ)(Fst)(p))((f)((Fst)(p))((Snd)(p)));
const NumericApplyToRange = (f)=>(m)=>(n)=>(Snd)((n)((NumericApplyPhi)(f))((V)(Once)(m)));
const Factorial = (NumericApplyToRange)(Mult)(Once);
const NumericSum = (NumericApplyToRange)(Add)(Zero);
const NumericSumRange = (m)=>(n)=>(Sub)((NumericSum)(n))((NumericSum)(m));
const SuccB = (B)(B);
const SuccList = (b)=>((SuccB)(b))(C);
const Single = (Th);
const PhiList = (p)=>(V)((SuccB)((Fst)(p)))((SuccList)((Fst)(p))((Snd)(p)));
const ComposerListPairN = (n)=>(n)(PhiList)((V)(I)(Single));
const ComposerN = (B)(Fst)(ComposerListPairN);
const ListNData = (n)=>((Is0)(n))(Th)((Snd)((ComposerListPairN)(n)));
const ListN = (n)=>(B)(ComposerN)(Succ)(n)((V)(n))(ListNData)((Pred)(n));
const Length = (Fst);
const Data = (Snd);
const Nullp = (B)(Is0)(Length);
const Pairp = (B)(Not)(Nullp);
const GetLast = (n)=>((Pred)(n))(K)(KI);
const GetFirst = (n)=>(n)(K);
const Head = (l)=>(B)((Data)(l))(GetFirst)((Pred)((Length)(l)));
const Last = (l)=>(Eq)((Length)(l))(Once)((Head)(l))((B)((Data)(l))(GetLast)((Pred)((Length)(l))));
const Nth = (n)=>(l)=>((Data)(l))(((B)(Is0)(Pred)(n))((B)(GetFirst)(Pred)((Length)(l)))(((ComposerN)(n))((B1)(GetFirst)(Sub)((Length)(l))(n))((B)(GetLast)(Pred)(n))));
const Push = (e)=>(l)=>(Is0)((Length)(l))((ListN)(Once)(e))(((Data)(l))((ListN)((B)(Succ)(Length)(l))(e)));
const PopPhi = (l)=>(p)=>(V)((B)(Pred)(Fst)(p))((Push)((B)(Nth)(Fst)(p)(l))((Snd)(p)));
const Pop = (l)=>(Is0)((B)(Pred)(Length)(l))((ListN)(Zero))((Snd)((Sub)((Length)(l))(Twice)((PopPhi)(l))((V)((B)(Pred)(Length)(l))((ListN)(Once)((Last)(l))))));
const FilterPhi = (l)=>(f)=>(p)=>(V)((B)(Pred)(Fst)(p))( (f)((B)(Nth)(Fst)(p)(l))((Push)((B)(Nth)(Fst)(p)(l))((Snd)(p)))((Snd)(p)));
const Filter = (f)=>(l)=>(Snd)((Length)(l)((FilterPhi)(l)(f))((V)((Length)(l))((ListN)(Zero))));
const MapPhi = (l)=>(f)=>(p)=>(V)((B)(Pred)(Fst)(p))((Push)((f)((B)(Nth)(Fst)(p)(l)))((Snd)(p)));
const Map = (f)=>(l)=>(Is0)((Length)(l))(l)((Snd)((B)(Pred)(Length)(l)((MapPhi)(l)(f))((V)((B)(Pred)(Length)(l))((ListN)(Once)((B)(f)(Last)(l))))));
const ReversePhi = (l)=>(p)=>(V)((B)(Succ)(Fst)(p))((Push)((B)(Nth)(Fst)(p)(l))((Snd)(p)));
const Reverse = (l)=>(Is0)((B)(Pred)(Length)(l))(l)((Snd)((B)(Pred)(Length)(l)((ReversePhi)(l))((V)(Twice)((ListN)(Once)((Head)(l))))));
const FlipArgs = (C);
const Backward = (f)=>(B)(Reverse)((B)(f)(Reverse));
const BackwardAtomic = (f)=>(B)(f)(Reverse);
const FoldlPhi = (f)=>(l)=>(p)=>(V)((B)(Succ)(Fst)(p))((f)((Nth)((Fst)(p))(l))((Snd)(p)));
const Foldl = (f)=>(e)=>(l)=>(Is0)((Length)(l))(Zero)((Snd)(((Length)(l))((FoldlPhi)(f)(l))((V)(Once)(e))));
const Foldr = (f)=>(e)=>(l)=>(Is0)((Length)(l))(Zero)((BackwardAtomic)((Foldl)(f)(e))(l));
const Max = (Foldl)((a)=>(b)=>(Gt)(a)(b)(a)(b))(Zero);
const Min = (l)=>(Foldl)((a)=>(b)=>(Lt)(a)(b)(a)(b))((Head)(l))(l);
const Car = (Head);
const Cdr = (Pop);
const Cadr = (B)(Car)(Cdr);
const Caddr = (B)(Car)((Twice)(Cdr));
const Cadddr = (B)(Car)((Thrice)(Cdr));
const InsertPhi = (n)=>(e)=>(l)=>(p)=>(V)((B)(Pred)(Fst)(p))(((Eq)((Fst)(p))(n))((Push)((B)(Nth)(Fst)(p)(l))((Push)(e)((Snd)(p))))((Push)((B)(Nth)(Fst)(p)(l))((Snd)(p))));
const Insert = (n)=>(e)=>(l)=>(Is0)(n)((Push)(e)(l))((Gt)(n)((Length)(l))((Backward)((Push)(e))(l))((Snd)((Length)(l)((InsertPhi)(n)(e)(l))((V)((Length)(l))((ListN)(Zero))))));
const ErasePhi = (n)=>(l)=>(p)=>(V)((B)(Pred)(Fst)(p))(((Eq)((Fst)(p))(n))((Snd)(p))((Push)((B)(Nth)(Fst)(p)(l))((Snd)(p))));
const Erase = (n)=>(l)=>(Is0)((Length)(l))((ListN)(Zero))((Or)((Is0)(n))((Gt)(n)((Length)(l)))(l)((Snd)((Length)(l)((ErasePhi)(n)(l))((V)((Length)(l))((ListN)(Zero))))));

// -:- IMPURE -:- Applies 'f' to 'n'th fcn in list 'l', PERFORMING A SIDE EFFECT
const IMPURE_VoidMapPhi = l => f => n => {
  f(Nth(n)(l)); // SIDE EFFECT MAY OCCUR HERE
  return Succ(n);
};
// -:- IMPURE -:- Maps VOID fcn 'f' over list 'l' (generally for passing a printing fcn)
const VoidMap = f => l => { Length(l)(IMPURE_VoidMapPhi(l)(f))(Once); };



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
print("  => Is0(Ox5): ");
bshow(Is0(Ox5));
print("  => Is0(Ox0): ");
bshow(Is0(Ox0));

show("");
print("  => Eq(Ox2)(Ox8):  ");
bshow(Eq(Ox2)(Ox8));
print("  => Eq(Ox2)(Ox2):  ");
bshow(Eq(Ox2)(Ox2));
print("  => Lt(Ox2)(Ox8):  ");
bshow(Lt(Ox2)(Ox8));
print("  => Lt(Ox8)(Ox2):  ");
bshow(Lt(Ox8)(Ox2));
print("  => Lt(Ox8)(Ox8):  ");
bshow(Lt(Ox8)(Ox8));
print("  => Gt(Ox2)(Ox8):  ");
bshow(Gt(Ox2)(Ox8));
print("  => Gt(Ox8)(Ox2):  ");
bshow(Gt(Ox8)(Ox2));
print("  => Gt(Ox8)(Ox8):  ");
bshow(Gt(Ox8)(Ox8));
print("  => Leq(Ox2)(Ox8): ");
bshow(Leq(Ox2)(Ox8));
print("  => Leq(Ox8)(Ox2): ");
bshow(Leq(Ox8)(Ox2));
print("  => Leq(Ox8)(Ox8): ");
bshow(Leq(Ox8)(Ox8));
print("  => Geq(Ox2)(Ox8): ");
bshow(Geq(Ox2)(Ox8));
print("  => Geq(Ox8)(Ox2): ");
bshow(Geq(Ox8)(Ox2));
print("  => Geq(Ox8)(Ox8): ");
bshow(Geq(Ox8)(Ox8));

show("");
print("  => IsFactor(Ox3)(Oxc): ");
bshow(IsFactor(Ox2)(Ox4));
print("  => IsFactor(Ox3)(Oxd): ");
bshow(IsFactor(Ox2)(Ox7));
print("  => Evenp(Ox6): ");
bshow(Evenp(Ox6));
print("  => Evenp(Ox9): ");
bshow(Evenp(Ox9));
print("  => Oddp(Ox6):  ");
bshow(Oddp(Ox6));
print("  => Oddp(Ox9):  ");
bshow(Oddp(Ox9));

show("");
print("  => Add(Oxf)(Oxa):            ");
nshow(Add(Oxf)(Oxa));
print("  => Sub(Oxb)(Ox6):            ");
nshow(Sub(Oxb)(Ox6));
print("  => Mult(Ox3)(Ox7):           ");
nshow(Mult(Ox3)(Ox7));
print("  => Pow(Ox2)(Ox5):            ");
nshow(Pow(Ox2)(Ox5));
print("  => Div(Mult(Ox2)(Oxa))(Ox4): ");
nshow(Div(Mult(Ox2)(Oxa))(Ox4));
print("  => Log(Ox2)(Ox8):            ");
nshow(Log(Ox2)(Ox8));

show("");
print("  => Succ(Ox8): ");
nshow(Succ(Ox8));
print("  => Pred(Ox8): ");
nshow(Pred(Ox8));

show("");
print("  => Factorial(Ox5):  ");
nshow(Factorial(Ox5));
print("  => NumericSum(Oxa): ");
nshow(NumericSum(Oxa));
print("  => NumericSumRange(Ox5)(Oxa): ");
nshow(NumericSumRange(Ox5)(Oxa));



show("\n\n\nUsing The Purely-Fcnal \"ListN\" Data Structure:");

show("  => We have defined 2 lists:");
show("     (1) List of 5 Church Numerals:");
show("         List1 = ListN(Ox5) (Ox9) (Ox4) (Ox7) (Ox3) (Oxa);");
const List1 = ListN(Ox5) (Ox9) (Ox4) (Ox7) (Ox3) (Oxa);
show("     (2) Empty List:");
show("         List2 = ListN(Ox0);");
const List2 = ListN(Ox0);

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
print("  => Nth(Ox1)(List1): ");
nshow(Nth(Ox1)(List1));
print("  => Nth(Ox2)(List1): ");
nshow(Nth(Ox2)(List1));
print("  => Nth(Ox3)(List1): ");
nshow(Nth(Ox3)(List1));
print("  => Nth(Ox4)(List1): ");
nshow(Nth(Ox4)(List1));
print("  => Nth(Ox5)(List1): ");
nshow(Nth(Ox5)(List1));

show("\nSETTERS:");
print("  => Length(Push(Oxd)(List1)):      ");
nshow(Length(Push(Oxd)(List1)));
print("  => Head(Push(Oxd)(List1)):        ");
nshow(Head(Push(Oxd)(List1)));
print("  => Length(Pop(List1)):            ");
nshow(Length(Pop(List1)));
print("  => Head(Pop(List1)):              ");
nshow(Head(Pop(List1)));
print("  => Length(Push(Oxf)(List2)):      ");
nshow(Length(Push(Oxf)(List2)));
print("  => Head(Push(Oxf)(List2)):        ");
nshow(Head(Push(Oxf)(List2)));
print("  => Length(Pop(Push(Oxf)(List2))): ");
nshow(Length(Pop(Push(Oxf)(List2))));
print("  => Erase(Ox3)(List1)       = ");
VoidMap(nprint)(Erase(Ox3)(List1));
print("\n  => Insert(Ox3)(Oxc)(List1) = ");
VoidMap(nprint)(Insert(Ox3)(Oxc)(List1));
show("");

show("\nFILTER/MAP/VOIDMAP:");
show("  => We have defined more 2 lists:");
show("     (1) List of odd Church Numerals from List1:");
show("         OnlyOdds = Filter(Oddp)(List1);");
const OnlyOdds = Filter(Oddp)(List1);
show("     (2) List of 2 raised to each value in List1:");
show("         PowersOf2 = Map(Pow(Ox2))(List1);\n");
const PowersOf2 = Map(Pow(Ox2))(List1);
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

print("\n  => Pow(Ox2)(Ox3)           = ");
nshow(Pow(Ox2)(Ox3));
print("  => FlipArgs(Pow)(Ox2)(Ox3) = ");
nshow(FlipArgs(Pow)(Ox2)(Ox3));

print("  => Push(Oxf)(List1)           = ");
VoidMap(nprint)(Push(Oxf)(List1));
print("\n  => Backward(Push(Oxf))(List1) = ");
VoidMap(nprint)(Backward(Push(Oxf))(List1));
show("\n  => We have defined 1 more List: List3 = ListN(Ox2) (Ox2)(Ox3);");
const List3 = ListN(Ox2) (Ox2)(Ox3);
print("     -> Foldl(Pow)(Ox1)(List3)                 = ");
nprint(Foldl(Pow)(Ox1)(List3));
print("\n     -> BackwardAtomic(Foldl(Pow)(Ox1))(List3) = ");
nshow(BackwardAtomic(Foldl(Pow)(Ox1))(List3));

show("\nACCUMULATORS:");
show("  => Both Accumulators have already been shown, 1 more subtly so:");
print("     -> Foldl(Pow)(Ox1)(List3) = ");
nshow(Foldl(Pow)(Ox1)(List3));
print("     -> Foldr(Pow)(Ox1)(List3) = ");
nprint(Foldr(Pow)(Ox1)(List3));
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
nshow(Car(List1));
print("  => cdr(List1)    = ");
VoidMap(nprint)(Cdr(List1));
print("\n  => cadr(List1)   = ");
nshow(Cadr(List1));
print("  => caddr(List1)  = ");
nshow(Caddr(List1));
print("  => cadddr(List1) = ");
nshow(Cadddr(List1));

show("\nBye!\n");
