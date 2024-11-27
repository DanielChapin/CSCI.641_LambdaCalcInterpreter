# Transpiling from Human to UTLC

Transpiling is the 4th step of the process.

1. Tokenize (Lexical analysis)
   - Reference HuamLexer.hs for token types
2. Parse (Syntactic Analysis)
   - Reference HuamLexer.hs for the Human grammar
3. Validate (Semantic Analysis)
   1. Validate imports
   2. Check for overlapping definitions
4. Transpile
   1. Check for outputs
   2. Generate lambda terms for each output
      1. Whenever we stumble upon an identifier, we check if that identifier has a transpiled definition and if it doesn't then we recursively transpile that identifier.
      2. If the identifier isn't defined externally, but is instead a recursive call to the definition we're transpiling, we need to apply the Y-Combinator and reference the recursive calls as a call to the passed recursive function, including passing the argument function to the new call.
   3. (If specified) Write generated lambdas to a specified file
5. (If specified) Execution
   1. (If unloaded) Load/parse lambda terms
   2. If in non-interactive mode
      1. Run all lambda terms until completion
      2. Print results
   3. If in interactive mode
      1. Wait for and handle user interaction
      2. Pretty print and step lambda terms in accordance to user inputs

I'm going to leave semantic analysis to the transpiler because the semantics might change from target to target.
(ie. a transpiler working with de Bruijn Indices might not care about overlapping variable names whereas another might treat that as an error.)
