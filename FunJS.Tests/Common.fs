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
      let result = engine.Run(code)
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
// Add array closure test.
// Add support for get_Zero and get_One in custom classes.
// Add support for exceptions.
// Add support for list/set equality.
// Add support for variable name re-use (list problem).
// Add support for array/seq/list modules.
// Add support for events/observables.
// Add support for computation expressions.
// Add support for custom operators.
// Add support for mapped javascript methods. JavaScript<string -> unit>("console.log")? Or Attribute?
// Add support for tail recursive transformations into while loops.
// Add support for type checks?