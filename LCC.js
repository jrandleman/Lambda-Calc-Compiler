// Author: Jordan Randleman - LCC.js => Lambda-Calc-Compiler-JS !!!

// USING THE JS LAMBDA CALC REPL, INTERPRETER, & COMPILER:
//   (I) RESERVED CHARS: 
//         (1) WHITESPACE &: .\()@
//   (II) NAMING:
//          (1) ALL LAMBDAS BEGIN WITH A CAPITAL LETTER & ONLY CONSIST OF ALPHANUMERICS + '_'
//          (2) ALL DATA (ARGS IN LAMBDAS) ARE A SINGLE LOWERCASE LETTER
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

InsertPhi := \\nelp.V (B Pred Fst p) ((Eq(Fst p)n) (Push (B Nth Fst p l) (Push e(Snd p))) (Push (B Nth Fst p l) (Snd p)))
Insert := \\nel.Is0 n (Push e l) (Gt n (Length l) (Backward(Push e)l) (Snd(Length l (InsertPhi n e l)(V(Length l)(ListN Zero)))))

ErasePhi := \\nlp.V(B Pred Fst p)((Eq(Fst p)n) (Snd p) (Push(B Nth Fst p l)(Snd p)))
Erase := \\nl.Is0(Length l) (ListN Zero) (Or(Is0 n)(Gt n(Length l)) l (Snd(Length l (ErasePhi n l) (V(Length l)(ListN Zero)))))

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

var lib_loaded = false;
var print_loaded = false;
var church_loaded = false;

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
 * REPL HELPER FUNCTIONS
********************************************************************************/

// Returns whether or not an input contains a paricular command
const isCommand = (input, cmd) => input.substr(0,cmd.length).includes(cmd);

// Outputs REPL's history buffer + Bolded Line Numbers
const showHistory = replBuff => {
  if(replBuff.length === 0) {
    printLn("lci> \x1b[1m\x1b[38;5;226mNO REPL HISTORY!\x1b[0m");
    return;
  }
  let lineNumber = 0; 
  const maxLineNumberDigits = (replBuff.split('\n').length-1).toString().length;
  printLn("lci> \x1b[1mREPL HISTORY:\x1b[0m");
  printLn("=".repeat(80));
  printLn(
    ('\n'+replBuff.trim()).replace(/\n/g, 
      () => `\n\x1b[1m${(++lineNumber).toString().padStart(maxLineNumberDigits,'0')})\x1b[0m `));
  printLn("=".repeat(80));
}

// Displays REPL's commands upon being invoked by "HELP" REPL command
const showReplHelpMessage = () => {
  printLn("lci> \x1b[1mREPL COMMANDS (case-insensitive):\x1b[0m");
  printLn("      (0) 'lci> EXIT'        exit REPL");
  printLn("      (1) 'lci> SYNTAX-HELP' show Lambda Calculus syntax guide");
  printLn("      (2) 'lci> HELP'        show this message");
  printLn("lci> \x1b[1mREPL FILE COMPILATION & LOADING:\x1b[0m");
  printLn("      (0) 'lci> LCC filename'     compile + exit");
  printLn("      (1) 'lci> LCI filename'     interpret/evaluate + exit");
  printLn("      (2) 'lci> COMPILE filename' compile");
  printLn("      (3) 'lci> LOAD filename'    load into REPL's history buffer");
  printLn("lci> \x1b[1mREPL HISTORY MANIPULATION:\x1b[0m");
  printLn("      (0) 'lci> SHOW-HISTORY'               print REPL history");
  printLn("      (1) 'lci> SAVE-HISTORY filename'      save REPL history to \"filename\"");
  printLn("      (2) 'lci> CLEAR-HISTORY'              clear REPL history");
  printLn("      (3) 'lci> DELETE lineNumber'          delete code at \"lineNumber\"");
  printLn("      (4) 'lci> REPLACE lineNumber newCode' rewrite \"lineNumber\" w/ \"newCode\"");
  printLn("      (5) 'lci> INSERT lineNumber newCode'  insert \"newCode\" \x1b[1mPRIOR\x1b[0m \"lineNumber\"");
  printLn("lci> \x1b[1mREPL DEFAULT LIBRARY:\x1b[0m");
  printLn("      (0) 'lci> LOAD-LIB'    load lambdas from \"JS_LambdaCalc_SampleExec.js\"");
  printLn("      (1) 'lci> LOAD-PRINT'  load printing \"show\" lambdas");
  printLn("      (2) 'lci> LOAD-CHURCH' load church numeral lambdas");
  printLn("lci> \x1b[1mREPL CODING:\x1b[0m");
  printLn("      (0) 'lci> @JS your_JS_expr' run \"your_JS_expr\" as JavaScript");
  printLn("      (1) 'lci> your_LC_expr'     interpret & run your Lambda Calculus!"); 
}

// Displays Lambda Calculus Syntax Guide
const showSyntaxHelpMessage = () => {
  printLn("lci> \x1b[1mREPL LAMBDA CALCULUS SYNTAX:\x1b[0m");
  printLn("     \x1b[1m(I)   RESERVED CHARS:\x1b[0m");
  printLn("            (0) WHITESPACE &: .\\()@");
  printLn("     \x1b[1m(II)  NAMING:\x1b[0m");
  printLn("            (0) LAMBDAS START W/ CAPITAL LETTER & ONLY HAVE ALPHANUMERICS + '_'");
  printLn("            (1) DATA (ARGS IN LAMBDAS) ARE A SINGLE LOWERCASE LETTER");
  printLn("     \x1b[1m(III) LAMBDA STRUCTURE:\x1b[0m");
  printLn("            (0) LambdaName := \\<args>.<returned operation>");
  printLn("            (1) IMMUTABLE");
  printLn("            (2) CURRY ARGS");
  printLn("            (3) RETURN THEIR BODY");
  printLn("     \x1b[1m(IV)  EMBEDDING JS IN A COMPILED/INTERPRETED FILE:\x1b[0m");
  printLn("            (0) SWAP BTWN \"LAMBDA CALCULUS\" & \"JAVASCRIPT\" SCOPES VIA TAGS:");
  printLn("                => '@JS' = ALL FOLLOWING CODE IS JAVASCRIPT TO BE LEFT AS IS");
  printLn("                => '@LC' = ALL FOLLOWING CODE IS LAMBDA CALC TO BE CONVERTED");
  printLn("                => IE: @JS");
  printLn("                         .. your JS here ..");
  printLn("                       @LC");
}

// Displays an error if given an invalid line # for the REPL history buffer,
// and returns whether or not said error was encountered
const validReplLineNo = (lineNo, replBuff, insertingLine = 0) => {
  const totalLines = replBuff.split('\n').length-1;
  if(isNaN(lineNo) || lineNo < 1 || lineNo > (totalLines+insertingLine)) { // totalLines+1 enables appending buffer via "insert"
    printLn("lci> \x1b[1m\x1b[31mREPL ERROR:\x1b[0m\x1b[1m Invalid Line Number!\x1b[0m");
    const err = "Line Number is " + (isNaN(lineNo) ? "Nan" : (lineNo < 1) 
                                      ? "< 1" : ("> total lines in buffer (" + totalLines + ")")) + "!";
    printLn(" ".repeat(17) + err);
    return false;
  }
  return true;
}

// Deletes the given lineNo from replBuffer (if exists, else outputs an error)
// Returns current replBuffer, post-deletion or error detection
const deleteLine = (lineNo, replBuff) => {
  if(replBuff.length === 0) {
    printLn("lci> \x1b[1m\x1b[38;5;226mNO REPL HISTORY!\x1b[0m");
  } else {
    if(!validReplLineNo(lineNo, replBuff)) return replBuff;
    let bufferLines = ('\n'+replBuff.trim()).split('\n');
    bufferLines.splice(lineNo,1); // rm line specified
    replBuff = bufferLines.join('\n')+'\n';
    printLn("lci> \x1b[1mLine " + lineNo + " Deleted!\x1b[0m");
  }
  return replBuff;
}

// Replaces the given lineNo from replBuffer (if exists, else outputs an error)
// Returns current replBuffer, post-replacement or error detection
const replaceLine = (lineNo, newCode, replBuff) => {
  if(replBuff.length === 0) {
    printLn("lci> \x1b[1m\x1b[38;5;226mNO REPL HISTORY!\x1b[0m");
  } else {
    if(!validReplLineNo(lineNo, replBuff)) return replBuff;
    let bufferLines = ('\n'+replBuff.trim()).split('\n');
    bufferLines[lineNo] = executeInterpreter(newCode); // replace line specified
    replBuff = bufferLines.join('\n')+'\n';
    printLn("lci> \x1b[1mLine " + lineNo + " Replaced!\x1b[0m");
  }
  return replBuff;
}

// Inserts code BEFORE the given lineNo from replBuffer (if exists, else outputs an error)
// Returns current replBuffer, post-insertion or error detection
const insertLine = (lineNo, newCode, replBuff) => {
  if(replBuff.length === 0) {
    printLn("lci> \x1b[1m\x1b[38;5;226mNO REPL HISTORY!\x1b[0m");
  } else {
    if(!validReplLineNo(lineNo, replBuff,1)) return replBuff;
    let bufferLines = ('\n'+replBuff.trim()).split('\n');
    bufferLines.splice(lineNo, 0, executeInterpreter(newCode)); // insert line specified
    replBuff = bufferLines.join('\n')+'\n';
    printLn("lci> \x1b[1mLine " + lineNo + " Inserted!\x1b[0m");
  }
  return replBuff;
}

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

  // CMD = EXIT | HELP | SYNTAX-HELP | SHOW-HISTORY
  const possible_command = line.toUpperCase().trim();
  switch(possible_command) {
    case "EXIT": repl.close(); // launch close method
    case "HELP": showReplHelpMessage(); repl.prompt(); return;
    case "SYNTAX-HELP": showSyntaxHelpMessage(); repl.prompt(); return;
    case "SHOW-HISTORY": showHistory(replBuffer); repl.prompt(); return;
  }

  // CMD = SAVE-HISTORY
  if(isCommand(possible_command, "SAVE-HISTORY")) {
    if(replBuffer.length === 0) {
      printLn("lci> \x1b[1m\x1b[38;5;226mNO REPL HISTORY!\x1b[0m");
    } else {
      const filename = line.replace(/[S|s][A|a][V|v][E|e]-[H|h][I|i][S|s][T|t][O|o][R|r][Y|y]/, '').trim(); // lol
      fs.writeFile(filename,replBuffer,(err, data) => {
        if(err) printLn('lci> \x1b[1m\x1b[31mREPL ERROR:\x1b[0m'), printLn(err);
        else printLn('\x1b[1mWritten to ' + filename + '!\x1b[0m');
        repl.prompt(); return;
      });
    }
    repl.prompt(); return;
  }

  // CMD = CLEAR-HISTORY
  if(isCommand(possible_command, "CLEAR-HISTORY")) {
    lib_loaded = print_loaded = church_loaded = false;
    replBuffer = [];
    printLn('lci> \x1b[1mHistory Cleared!\x1b[0m');
    repl.prompt(); return;
  }

  // CMD = DELETE LINE
  if(isCommand(possible_command,"DELETE ")) {
    const lineNumber = parseInt(line.replace(/[D|d][E|e][L|l][E|e][T|t][E|e] /, '').trim(), 10);
    replBuffer = deleteLine(lineNumber,replBuffer);
    repl.prompt(); return;
  }

  // CMD = REPLACE LINE
  if(isCommand(possible_command,"REPLACE ")) {
    const lineNumber = parseInt(line.replace(/[R|r][E|e][P|p][L|l][A|a][C|c][E|e] /, '').trim(), 10);
    let newCodeLines = line.replace(/[R|r][E|e][P|p][L|l][A|a][C|c][E|e] /, '').trim().split(' ');
    newCodeLines.shift(); // rm line number from input
    replBuffer = replaceLine(lineNumber,newCodeLines.join(' '),replBuffer);
    repl.prompt(); return;
  }

  // CMD = INSERT __BEFORE__ LINE
  if(isCommand(possible_command,"INSERT ")) {
    const lineNumber = parseInt(line.replace(/[I|i][N|n][S|s][E|e][R|r][T|t] /, '').trim(), 10);
    let newCodeLines = line.replace(/[I|i][N|n][S|s][E|e][R|r][T|t] /, '').trim().split(' ');
    newCodeLines.shift(); // rm line number from input
    replBuffer = insertLine(lineNumber,newCodeLines.join(' '),replBuffer);
    repl.prompt(); return;
  }

  // CMD = COMPILE FILE
  if(isCommand(possible_command,"LCC ") || isCommand(possible_command,"COMPILE ")) {
    const filename = (isCommand(possible_command,"COMPILE ")) ? 
                      line.replace(/[C|c][O|o][M|m][P|p][I|i][L|l][E|e] /, '').trim() : 
                      line.replace(/[L|l][C|c][C|c] /, '').trim();
    fs.readFile(filename, 'utf-8', (err, data) => {
      if(err) throw err;
      fs.writeFile(`LCC_${filename}`,executeInterpreter(data),(err, data) => {
        if(err) throw err;
        printLn(`\x1b[1mCompiled ${filename} => LCC_${filename}!\x1b[0m`);
        if(isCommand(possible_command,"LCC ")) repl.close();
        repl.prompt(); return;
      });
    });
    repl.prompt(); return;
  }

  // CMD = INTERPRET FILE
  if(isCommand(possible_command,"LCI ") || isCommand(possible_command,"LOAD ")) {
    const filename = isCommand(possible_command,"LOAD ") ? 
                      line.replace(/[L|l][O|o][A|a][D|d] /, '').trim() : 
                      line.replace(/[L|l][C|c][I|i] /, '').trim();
    printLn(`lci> \x1b[1mInterpreting ${filename} ..\x1b[0m`);
    fs.readFile(filename, 'utf-8', (err, data) => {
      if(err) throw err;
      if(isCommand(possible_command,"LCI ")) {
        eval(executeInterpreter(data));
        repl.close();
      }
      replBuffer += executeInterpreter(data) + '\n';
      repl.prompt(); return;
    });
    repl.prompt(); return;
  }

  // CMD = LOAD-LIB(rary)
  if(isCommand(possible_command,"LOAD-LIB")) {
    lib_loaded = true;
    let loadable_lib = LCC_LIBRARY;
    if(print_loaded) 
      loadable_lib = loadable_lib.slice(0,LCC_LIBRARY.indexOf('Ox0')) + 
                     loadable_lib.slice(LCC_LIBRARY.indexOf('Oxe fa')+7);
    if(church_loaded)
      loadable_lib = loadable_lib.slice(LCC_LIBRARY.indexOf('@LC') + 3);
    replBuffer += executeInterpreter(loadable_lib) + '\n';
    printLn("lci> \x1b[1mLibrary Loaded!\x1b[0m");
    repl.prompt(); return;
  }

  // CMD = LOAD-PRINT(ing fcns)
  if(isCommand(possible_command,"LOAD-PRINT")) {
    if(lib_loaded) {repl.prompt(); return;} // lib encapsulates printing fcns
    print_loaded = true;
    replBuffer += executeInterpreter(LCC_LIBRARY.slice(0,LCC_LIBRARY.indexOf('@LC') + 3)) + '\n';
    printLn("lci> \x1b[1mPrinting Lambdas Loaded!\x1b[0m");
    repl.prompt(); return;
  }

  // CMD = LOAD-CHURCH( numerals)
  if(isCommand(possible_command,"LOAD-CHURCH")) {
    if(lib_loaded) {repl.prompt(); return;} // lib encapsulates church numeral lambdas
    church_loaded = true; 
    replBuffer += executeInterpreter(LCC_LIBRARY.slice(LCC_LIBRARY.indexOf('Ox0'),LCC_LIBRARY.indexOf('Oxe fa') + 7)) + '\n';
    printLn("lci> \x1b[1mChurch Numerals Loaded!\x1b[0m");
    repl.prompt(); return;
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
    printLn('lci> \x1b[1m\x1b[31mREPL ERROR:\x1b[0m');
    printLn(err);
    repl.prompt();
  }

}).on('close', () => { // exit the program successfully on 'close' method
  printLn("lci> \x1b[1mBluebird Id!\x1b[0m"); // B(I)!
  process.exit(0);
});
