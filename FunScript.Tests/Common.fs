[<AutoOpen>]
module FunScript.Tests.Common

open FunScript
open Jint
open NUnit.Framework
open Microsoft.FSharp.Linq.QuotationEvaluation

let checkAreEqual expectedResult quote =
   let code = Compiler.compile quote
   try
      let engine = JintEngine()
      let result = engine.Run(code + "\nreturn null;")
      Assert.That((result = expectedResult))
   // Wrap xUnit exceptions to stop pauses.
   with ex ->
      printfn "// Code:\n%s" code
      if ex.GetType().Namespace.StartsWith "FunScript" then raise ex
      else failwithf "Message: %s\n" ex.Message

/// Bootstrapping:
/// Generates code. Runs it through a JS interpreter. 
/// Checks the result against the compiled expression.
let check (quote:Quotations.Expr) =
   let expectedResult = quote.EvalUntyped()
   checkAreEqual expectedResult quote

// TODO:
// Add support for inheritance.
// Add support for TypeScript inheritance.
// Add support for TypeScript param arrays.
// Add support for TypeScript optional members/params.
// Add tests for union/array/list/seq/map/set equality/comparison.
// Add support for multiple constructors.
// Add support for method name overloading. DefineGlobal to take MethodBase? What about instances?
// Add support for type checks. How would you differentiate between ints and floats though?
// Add support for renaming reserved words.
// Add support for exceptions.
// Add support for events/observables.
// Add support for computation expressions.
// Add support for custom operators.
// Add support for tail recursive transformations into while loops.