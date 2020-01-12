@JS
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
@LC

I := \a.a
K := \ab.a
KI := K I
C := \fab.fba

True := K
False := KI

Not := C
And := \pq.pqp
Or := \pq.ppq
Xor := \pq.p(Not q)q
Beq := \pq.pq(Not q)

B := \fga.f(ga)
B1 := B B B
Th := C I

Zero := False
Once := I
Twice := \fa.f(fa)
Thrice := \fa.f(f(fa))
Fourfold := \fa.f(f(f(fa)))
Fivefold := \fa.f(f(f(f(fa))))

Ox0 := \fa.a
Ox1 := \fa.f(Ox0 fa)
Ox2 := \fa.f(Ox1 fa)
Ox3 := \fa.f(Ox2 fa)
Ox4 := \fa.f(Ox3 fa)
Ox5 := \fa.f(Ox4 fa)
Ox6 := \fa.f(Ox5 fa)
Ox7 := \fa.f(Ox6 fa)
Ox8 := \fa.f(Ox7 fa)
Ox9 := \fa.f(Ox8 fa)
Oxa := \fa.f(Ox9 fa)
Oxb := \fa.f(Oxa fa)
Oxc := \fa.f(Oxb fa)
Oxd := \fa.f(Oxc fa)
Oxe := \fa.f(Oxd fa)
Oxf := \fa.f(Oxe fa)

Succ := \nf.B f(nf)
Add := \nk.k(Succ)n
Mult := B
Pow := Th

V := B C Th
Fst := \p.p K
Snd := \p.p KI
Phi := \p.V(Snd p)(B Succ Snd p)

Pred := \n.Fst(n Phi (V Zero Zero))
Sub := \nk.k Pred n

Is0 := \n.n(K False) True
Leq := \nk.B1 Is0 Sub nk
Eq := \nk.And(Leq nk)(Leq kn)
Geq := \nk.B1 Is0 Sub kn
Gt := \nk.Not(Leq nk)
Lt := \nk.Not(Geq nk)



IsFactorPhi := \mp.V (Eq(B Succ Fst p)m Zero (B Succ Fst p)) (Eq(B Succ Fst p)m)
IsFactor := \mn.Is0 m False (Is0 n True (B Is0 Pred m True (Gt mn False (Eq mn True (Snd (n (IsFactorPhi m) (V Zero True)))))))

Evenp := IsFactor Twice
Oddp := B1 Not IsFactor Twice

NumericApplyPhi := \fp.V(B Succ Fst p)(f(Fst p)(Snd p))
NumericApplyToRange := \fmn.Snd(n(NumericApplyPhi f)(V Once m))

Factorial := NumericApplyToRange Mult Once
NumericSum := NumericApplyToRange Add Zero
NumericSumRange := \mn.Sub(NumericSum n)(NumericSum m)



SuccB := B B
SuccList := \b.(SuccB b) C
Single := Th

PhiList := \p.V (SuccB (Fst p)) (SuccList (Fst p) (Snd p))
ComposerListPairN := \n.n PhiList (V I Single)
ComposerN := B Fst ComposerListPairN
ListNData := \n.(Is0 n) Th (Snd (ComposerListPairN n))

ListN := \n.B ComposerN Succ n (V n) ListNData (Pred n) 
Length := Fst
Data := Snd
Nullp := B Is0 Length
Pairp := B Not Nullp

GetLast := \n.(Pred n) K KI
GetFirst := \n.n K
Head := \l.B (Data l) GetFirst (Pred (Length l))
Last := \l.Eq(Length l)Once (Head l) (B(Data l)GetLast(Pred(Length l)))
Nth := \nl.(Data l)((B Is0 Pred n)(B GetFirst Pred (Length l))((ComposerN n)(B1 GetFirst Sub (Length l) n)(B GetLast Pred n)))

Push := \el.Is0(Length l)(ListN Once e)((Data l)(ListN(B Succ Length l)e))
PopPhi := \lp.V(B Pred Fst p)(Push(B Nth Fst pl)(Snd p))
Pop := \l.Is0(B Pred Length l)(ListN Zero)(Snd(Sub(Length l)Twice (PopPhi l) (V(B Pred Length l)(ListN Once (Last l)))))

FilterPhi := \lfp.V(B Pred Fst p)( f(B Nth Fst pl) (Push(B Nth Fst pl)(Snd p)) (Snd p))
Filter := \fl.Snd (Length l (FilterPhi lf) (V (Length l) (ListN Zero)))

MapPhi := \lfp.V(B Pred Fst p)(Push(f(B Nth Fst pl))(Snd p))
Map := \fl.Is0(Length l)l(Snd(B Pred Length l(MapPhi lf)(V (B Pred Length l)(ListN Once(B f Last l)))))

ReversePhi := \lp.V(B Succ Fst p)(Push(B Nth Fst pl)(Snd p))
Reverse := \l.Is0(B Pred Length l)l(Snd(B Pred Length l(ReversePhi l)(V Twice(ListN Once(Head l)))))

FlipArgs := C
Backward := \f.B Reverse(B f Reverse)
BackwardAtomic := \f.B f Reverse

FoldlPhi := \flp.V(B Succ Fst p)(f(Nth(Fst p)l)(Snd p))
Foldl := \fel.Is0(Length l)Zero(Snd((Length l)(FoldlPhi f l)(V Once e)))
Foldr := \fel.Is0(Length l)Zero(BackwardAtomic(Foldl fe)l)

Max := Foldl(\ab.Gt abab)Zero
Min := \l.Foldl(\ab.Lt abab)(Head l)l

Car := Head
Cdr := Pop
Cadr := B Car Cdr
Caddr := B Car(Twice Cdr)
Cadddr := B Car(Thrice Cdr)

InsertPhi := \nelp.V (B Pred Fst p) ((Eq(Fst p)n) (Push (B Nth Fst p l) (Push e(Snd p))) (Push (B Nth Fst p l) (Snd p)))
Insert := \nel.Is0 n (Push e l) (Gt n (Length l) (Backward(Push e)l) (Snd(Length l (InsertPhi n e l)(V(Length l)(ListN Zero)))))

ErasePhi := \nlp.V(B Pred Fst p)((Eq(Fst p)n) (Snd p) (Push(B Nth Fst p l)(Snd p)))
Erase := \nl.Is0(Length l) (ListN Zero) (Or(Is0 n)(Gt n(Length l)) l (Snd(Length l (ErasePhi n l) (V(Length l)(ListN Zero)))))

@JS
// -:- IMPURE -:- Applies 'f' to 'n'th fcn in list 'l', PERFORMING A SIDE EFFECT
const IMPURE_VoidMapPhi = l => f => n => {
  f(Nth(n)(l)); // SIDE EFFECT MAY OCCUR HERE
  return Succ(n);
};
// -:- IMPURE -:- Maps VOID fcn 'f' over list 'l' (generally for passing a printing fcn)
const VoidMap = f => l => { Length(l)(IMPURE_VoidMapPhi(l)(f))(Once); };
@LC










@JS

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
print("  => Add(Oxf)(Oxa):  ");
nshow(Add(Oxf)(Oxa));
print("  => Sub(Oxb)(Ox6):  ");
nshow(Sub(Oxb)(Ox6));
print("  => Mult(Ox3)(Ox7): ");
nshow(Mult(Ox3)(Ox7));
print("  => Pow(Ox2)(Ox5):  ");
nshow(Pow(Ox2)(Ox5));

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
