[<AutoOpen>]
module FunJS.Tests.Common

open FunJS
open FsUnit.Xunit
open Jint
open Linq.QuotationEvaluation

let checkAreEqual expectedResult quote =
   let code = Compiler.compile quote
   printfn "// Code:\n%s" code
   try
      let engine = JintEngine()
      let result = engine.Run(code + "\nreturn null;")
      result |> should equal expectedResult
   // Wrap xUnit exceptions to stop pauses.
   with ex ->
      if ex.GetType().Namespace.StartsWith "FunJS" then raise ex
      else failwithf "Message: %s\n" ex.Message

/// Bootstrapping:
/// Generates code. Runs it through a JS interpreter. 
/// Checks the result against the compiled expression.
let check (quote:Quotations.Expr) =
   let expectedResult = quote.EvalUntyped()
   checkAreEqual expectedResult quote

// TODO:
// Fix TypeScript provider for case where interface and class have overlapping properties.
// Add support for type checks. How would you differentiate between ints and floats though?
// Add support for multiple constructors.
// Add support for array equality.
// Add support for renaming reserved words.
// Add support for exceptions.
// Add support for events/observables.
// Add support for computation expressions.
// Add support for custom operators.
// Add support for tail recursive transformations into while loops.