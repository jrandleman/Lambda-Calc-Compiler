// Author: Jordan Randleman - LCC.js => Lambda-Calc-Compiler-JS !!!

// USING THE JS LAMBDA CALC REPL, INTERPRETER, & COMPILER:
//   (I) RESERVED CHARS: 
//         (1) WHITESPACE &: .\()@
//   (II) NAMING:
//          (1) ALL LAMBDAS BEGIN WITH A CAPITAL LETTER & ONLY CONSIST OF ALPHANUMERICS + '_'
//          (2) ALL DATA (ARGS W/IN FCNS) ARE A SINGLE LOWERCASE LETTER
//   (III) LAMBDA STRUCTURE:
//           (1) LambdaName := \<args>.<returned operation>
//           (2) ALL LAMBDAS ARE IMMUTABLE
//           (3) ALL LAMBDAS CURRY ARGS
//           (4) ALL LAMBDAS RETURNS THEIR BODY
//   (IV) EMBEDDING JS:
//          (1) SWAP BTWN "LAMBDA CALCULUS" & "JAVASCRIPT" SCOPES VIA TAGS:
//              => '@JS' = ALL FOLLOWING CODE IS JAVASCRIPT TO BE LEFT AS IS
//              => '@LC' = ALL FOLLOWING CODE IS LAMBDA CALC TO BE CONVERTED
//              => IE: @JS
//                       .. your JS here ..
//                     @LC

const LCC_LIBRARY = 
`@JS
// custom_list_from_cpp.js

// Show any data
const show = console.log;

// Lambdas for visualizing fcnal booleans
const bshow = f => show(f(true)(false));

// Lambdas for visualizing church numerals
const nshow = f => show((f(x => x + 1))(0));
@LC

I := \\a.a
K := \\ab.a
KI := K I
C := \\fab.fba

True := K
False := KI

Not := C
And := \\pq.pqp
Or := \\pq.ppq
Xor := \\pq.p(Not q)q
Beq := \\pq.pq(Not q)

B := \\fga.f(ga)
B1 := B B B
Th := C I

Zero := False
Once := I
Twice := \\fa.f(fa)
Thrice := \\fa.f(f(fa))
Fourfold := \\fa.f(f(f(fa)))
Fivefold := \\fa.f(f(f(f(fa))))

Ox0 := \\fa.a
Ox1 := \\fa.f(Ox0 fa)
Ox2 := \\fa.f(Ox1 fa)
Ox3 := \\fa.f(Ox2 fa)
Ox4 := \\fa.f(Ox3 fa)
Ox5 := \\fa.f(Ox4 fa)
Ox6 := \\fa.f(Ox5 fa)
Ox7 := \\fa.f(Ox6 fa)
Ox8 := \\fa.f(Ox7 fa)
Ox9 := \\fa.f(Ox8 fa)
Oxa := \\fa.f(Ox9 fa)
Oxb := \\fa.f(Oxa fa)
Oxc := \\fa.f(Oxb fa)
Oxd := \\fa.f(Oxc fa)
Oxe := \\fa.f(Oxd fa)
Oxf := \\fa.f(Oxe fa)

Succ := \\nf.B f(nf)
Add := \\nk.k(Succ)n
Mult := B
Pow := Th

V := B C Th
Fst := \\p.p K
Snd := \\p.p KI
Phi := \\p.V(Snd p)(B Succ Snd p)

Pred := \\n.Fst(n Phi (V Zero Zero))
Sub := \\nk.k Pred n

Is0 := \\n.n(K False) True
Leq := \\nk.B1 Is0 Sub nk
Eq := \\nk.And(Leq nk)(Leq kn)
Geq := \\nk.B1 Is0 Sub kn
Gt := \\nk.Not(Leq nk)
Lt := \\nk.Not(Geq nk)



IsFactorPhi := \\mp.V (Eq(B Succ Fst p)m Zero (B Succ Fst p)) (Eq(B Succ Fst p)m)
IsFactor := \\mn.Is0 m False (Is0 n True (B Is0 Pred m True (Gt mn False (Eq mn True (Snd (n (IsFactorPhi m) (V Zero True)))))))

Evenp := IsFactor Twice
Oddp := B1 Not IsFactor Twice


NumericApplyPhi := \\fp.V(B Succ Fst p)(f(Fst p)(Snd p))
NumericApplyToRange := \\fmn.Snd(n(NumericApplyPhi f)(V Once m))

Factorial := NumericApplyToRange Mult Once
NumericSum := NumericApplyToRange Add Zero
NumericSumRange := \\mn.Sub(NumericSum n)(NumericSum m)



SuccB := B B
SuccList := \\b.(SuccB b) C
Single := Th

PhiList := \\p.V (SuccB (Fst p)) (SuccList (Fst p) (Snd p))
ComposerListPairN := \\n.n PhiList (V I Single)
ComposerN := B Fst ComposerListPairN
ListNData := \\n.(Is0 n) Th (Snd (ComposerListPairN n))

ListN := \\n.B ComposerN Succ n (V n) ListNData (Pred n) 
Length := Fst
Data := Snd
Nullp := B Is0 Length
Pairp := B Not Nullp

GetLast := \\n.(Pred n) K KI
GetFirst := \\n.n K
Head := \\l.B (Data l) GetFirst (Pred (Length l))
Last := \\l.Eq(Length l)Once (Head l) (B(Data l)GetLast(Pred(Length l)))
Nth := \\nl.(Data l)((B Is0 Pred n)(B GetFirst Pred (Length l))((ComposerN n)(B1 GetFirst Sub (Length l) n)(B GetLast Pred n)))

Push := \\el.Is0(Length l)(ListN Once e)((Data l)(ListN(B Succ Length l)e))
PopPhi := \\lp.V(B Pred Fst p)(Push(B Nth Fst pl)(Snd p))
Pop := \\l.Is0(B Pred Length l)(ListN Zero)(Snd(Sub(Length l)Twice (PopPhi l) (V(B Pred Length l)(ListN Once (Last l)))))

FilterPhi := \\lfp.V(B Pred Fst p)( f(B Nth Fst pl) (Push(B Nth Fst pl)(Snd p)) (Snd p))
Filter := \\fl.Snd (Length l (FilterPhi lf) (V (Length l) (ListN Zero)))

MapPhi := \\lfp.V(B Pred Fst p)(Push(f(B Nth Fst pl))(Snd p))
Map := \\fl.Is0(Length l)l(Snd(B Pred Length l(MapPhi lf)(V (B Pred Length l)(ListN Once(B f Last l)))))

ReversePhi := \\lp.V(B Succ Fst p)(Push(B Nth Fst pl)(Snd p))
Reverse := \\l.Is0(B Pred Length l)l(Snd(B Pred Length l(ReversePhi l)(V Twice(ListN Once(Head l)))))

FlipArgs := C
Backward := \\f.B Reverse(B f Reverse)
BackwardAtomic := \\f.B f Reverse

FoldlPhi := \\flp.V(B Succ Fst p)(f(Nth(Fst p)l)(Snd p))
Foldl := \\fel.Is0(Length l)Zero(Snd((Length l)(FoldlPhi f l)(V Once e)))
Foldr := \\fel.Is0(Length l)Zero(BackwardAtomic(Foldl fe)l)

Max := Foldl(\\ab.Gt abab)Zero
Min := \\l.Foldl(\\ab.Lt abab)(Head l)l

Car := Head
Cdr := Pop
Cadr := B Car Cdr
Caddr := B Car(Twice Cdr)
Cadddr := B Car(Thrice Cdr)

@JS
// -:- IMPURE -:- Applies 'f' to 'n'th fcn in list 'l', PERFORMING A SIDE EFFECT
const IMPURE_VoidMapPhi = l => f => n => {
  f(Nth(n)(l)); // SIDE EFFECT MAY OCCUR HERE
  return Succ(n);
};
// -:- IMPURE -:- Maps VOID fcn 'f' over list 'l' (generally for passing a printing fcn)
const VoidMap = f => l => { Length(l)(IMPURE_VoidMapPhi(l)(f))(Once); };
@LC
`;

const fs = require('fs');
const readline = require('readline');

/********************************************************************************
 * INTERPRETATION 
********************************************************************************/

// Converts Currying shorthand to explicit lambdas: '\abc.' => '\a.\b.\c.'
const curryLambdaArgs = buff => buff.replace(/\\(\w{2,})\./g, 
                                      (substr, match1) => match1.split('').map(arg => `\\${arg}.`)
                                    .join(''));
// Applies the Currying Parens
const applyBodyParens = (exp, expBodyIdx) => exp.slice(0,expBodyIdx) + exp.slice(expBodyIdx).replace(/(\w+)/g, '($1)');

// Converts Lambda body fcns to all Curry one another
const curryLambdaBodies = buff => buff.split(/\n/)
                                      .filter(exp => exp.length > 0)
                                      .map(exp => !exp.includes(':=') ? exp : applyBodyParens(exp, exp.indexOf(':=')+2))
                                      .join('\n')
                                      .replace(/\(([a-z|_|0-9]{2,})\)/g,(str,m1)=>`${m1.split('').map(e=>`(${e})`) // (fba) => (f)(b)(a)
                                                                                                     .join('')}`);
// Converts LC operators to JS syntax
const convertOperators = buff => buff.replace(/:=/g, '=').replace(/\\/g, '').replace(/\./g, '=>');

// Adds 'const's for immutability & ';' for concrete JS interpretation
const addConstAndSemicolons = buff => `const ${buff.replace(/\n/g, ';\nconst ')};`.replace(/const ;/g, '')
                                                                                  .trim()
                                                                                  .replace(/\) \(/g, ')(')
                                                                                  .replace(/(\s+);/g, ';');
// Composes Interpreter Fcns
const convertLambdaCalcToJS = buff => addConstAndSemicolons(convertOperators(curryLambdaBodies(curryLambdaArgs(buff))));

/********************************************************************************
 * EMBEDDED JS HANDLING + INTERPRETATION 
********************************************************************************/

// Saves Instances of embedded JS Code in "embeddedJS", where a code's idx in "embeddedJS"
//   corresponds to the # following a '#' in the buffer to mark its position
const registerEmbeddedJS = (embeddedJS, buff, embeddedCount = 0) => {
  buff = buff.replace(/@js/g, '@JS').replace(/@lc/g, '@LC'); // make language tags flush
  if(buff.split('@JS').length-1 > buff.split('@LC').length-1) buff += '\n@LC';
  return buff.replace(/@JS((.|\s)*?)@LC/g, (str, embedded) => {embeddedJS.push(embedded); return `#${(embeddedCount++).toString()}`});
}
// Reinserts registered embedded JS
const restoreEmbeddedJS = (embeddedJS, buff) => buff.replace(/const #(\d+);/g, (str, embeddedIdx) => embeddedJS[embeddedIdx])

// Launch Interpreter & return LS->JS converted buffer
//   => TAGS TO EMBED JS: @JS = running javascript, @LC = running Lambda Calc (defaults to @LC)
const executeInterpreter = (buff, embeddedJS = []) => restoreEmbeddedJS(embeddedJS,convertLambdaCalcToJS(registerEmbeddedJS(embeddedJS,buff)));

/********************************************************************************
 * LAUNCH REPL
********************************************************************************/

// Declare buffers to hold REPL history
let lineBuffer = ""; // holds line most recently input
let replBuffer = ""; // holds lines that didn't throw an error in 'eval()'


// Redefine "console.log" to returns its output, 
// so we know when to comment out REPL's user printing expressions
const printLn = console.log;
console.log = data => { printLn(data); return data; };


// Set up input process w/ a default prompt
const repl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: 'lci> ', // Lambda Calc Interpretter
});


// Prompt input
repl.prompt(); 


// Once '\n' registered
repl.on('line', line => { 

  // if commanded to exit, help, or show REPL history
  const possible_command = line.toUpperCase().trim();
  switch(possible_command) {
    case "EXIT": repl.close(); // launch close method
    case "HELP":
      printLn("lci> \x1b[1mREPL COMMANDS (case-insensitive):\x1b[0m");
      printLn("      (0) 'lci> EXIT' to exit,");
      printLn("      (1) 'lci> HELP' for this message,");
      printLn("      (2) 'lci> SHOW-HISTORY' to print REPL history,");
      printLn("      (3) 'lci> SAVE-HISTORY filename' to save REPL history to \"filename\"");
      printLn("lci> \x1b[1mREPL FILE COMPILATION & LOADING:\x1b[0m");
      printLn("      (0) 'lci> LCC filename' compile + exit,");
      printLn("      (1) 'lci> LCI filename' interpret/evaluate + exit,");
      printLn("      (2) 'lci> COMPILE filename' compile,");
      printLn("      (3) 'lci> LOAD filename' load into REPL's history buffer");
      printLn("lci> \x1b[1mREPL DEFAULT LIBRARY:\x1b[0m");
      printLn("      (0) 'lci> LOAD-LIB' load lambdas from \"JS_LambdaCalc_SampleExec.js\"");
      printLn("lci> \x1b[1mREPL CODING:\x1b[0m");
      printLn("      (0) 'lci> @JS your_JS_expr' to run \"your_JS_expr\" as JavaScript,");
      printLn("      (1) 'lci> your_LC_expr' to inertpret & run your Lambda Calculus!"); 
      repl.prompt();
      return;
    case "SHOW-HISTORY":
      switch(replBuffer.length) {
        case 0: printLn("lci> \x1b[1mNO REPL HISTORY!\x1b[0m");
        default: printLn("lci> \x1b[1mREPL HISTORY:\x1b[0m"), printLn("=".repeat(80)), printLn(replBuffer), printLn("=".repeat(80)); break;
      }
      repl.prompt();
      return;
  }

  // if commanded to save REPL history
  if(possible_command.includes("SAVE-HISTORY")) {
    if(replBuffer.length === 0) {
      printLn("lci> \x1b[1mNO REPL HISTORY!\x1b[0m");
    } else {
      const filename = line.replace(/[S|s][A|a][V|v][E|e]-[H|h][I|i][S|s][T|t][O|o][R|r][Y|y]/g, '').trim(); // Lol
      fs.writeFile(filename,replBuffer,(err, data) => {
        if(err) printLn('  =>\x1b[1mLCC.js> \x1b[31mREPL ERROR:\x1b[0m\x1b[0m'), printLn(err);
        else printLn('\x1b[1mWritten to ' + filename + '!\x1b[0m');
        repl.prompt();
        return;
      });
    }
    repl.prompt();
    return;
  }

  // if commanded to compile a file
  if(possible_command.includes("LCC ") || possible_command.includes("COMPILE ")) {
    const filename = (possible_command.includes("COMPILE ")) ? 
                      line.replace(/[C|c][O|o][M|m][P|p][I|i][L|l][E|e] /g, '').trim() : 
                      line.replace(/[L|l][C|c][C|c] /g, '').trim();
    fs.readFile(filename, 'utf-8', (err, data) => {
      if(err) throw err;
      fs.writeFile(`LCC_${filename}`,executeInterpreter(data),(err, data) => {
        if(err) throw err;
        printLn(`\x1b[1mCompiled ${filename} => LCC_${filename}!\x1b[0m`);
        if(possible_command.includes("LCC ")) repl.close();
        repl.prompt(); 
        return;
      });
    });
    repl.prompt(); 
    return;
  }

  // if commanded to interpret a file
  if(possible_command.includes("LCI ") || possible_command.includes("LOAD ")) {
    const filename = possible_command.includes("LOAD ") ? 
                      line.replace(/[L|l][O|o][A|a][D|d] /g, '').trim() : 
                      line.replace(/[L|l][C|c][I|i] /g, '').trim();
    printLn(`lci> \x1b[1mInterpreting ${filename} ..\x1b[0m`);
    fs.readFile(filename, 'utf-8', (err, data) => {
      if(err) throw err;
      if(possible_command.includes("LCI ")) {
        eval(executeInterpreter(data));
        repl.close();
      }
      replBuffer += executeInterpreter(data) + '\n';
      repl.prompt(); 
      return;
    });
    repl.prompt(); 
    return;
  }

  // if commanded to import premade lambda-calc library
  if(possible_command.includes("LOAD-LIB")) {
    replBuffer += executeInterpreter(LCC_LIBRARY) + '\n';
    printLn("lci> \x1b[1mLibrary Loaded!\x1b[0m");
    repl.prompt(); 
    return;
  }

  // try evaluating input
  lineBuffer = line.toUpperCase().includes('@JS') 
                ? line.replace(/@[J|j][S|s]/g,'') 
                : executeInterpreter(line);
  try { 
    switch(eval(replBuffer + lineBuffer)) {
      case undefined: replBuffer += lineBuffer + '\n'; break; // no output hence save data
      default: replBuffer += '/*' + lineBuffer + '*/\n';      // output hence comment out "outputting" expression
    }
    repl.prompt(); // reprompt to retrigger 'line' method
  } catch(err) {
    printLn('  =>\x1b[1mLCC.js> \x1b[31mREPL ERROR:\x1b[0m\x1b[0m');
    printLn(err);
    repl.prompt();
  }

}).on('close', () => { // exit the program successfully on 'close' method
  printLn("lci> \x1b[1mBluebird Id!\x1b[0m"); // B(I)
  process.exit(0);
});
