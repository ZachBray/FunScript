[<AutoOpen>]
module FunScript.Tests.Common

open FunScript
open Jint
open NUnit.Framework
open Microsoft.FSharp.Linq.QuotationEvaluation


[<JSEmit("test_log({0}.toString());")>]
let log (msg : obj) : unit = failwith "never"

let defaultCompile quote =
    Compiler.Compiler.Compile(quote, noReturn = true(*, shouldCompress = true*))

let compileWithComponents components quote =
    Compiler.Compiler.Compile(quote, components = components, noReturn = true(*, shouldCompress = true*))

let compileWithRx quote =
      Compiler.Compiler.Compile(quote, components = Rx.Interop.components(), noReturn = true, isEventMappingEnabled = false(*, shouldCompress = true*))

let checkAreEqualWith prerequisiteJS compile expectedResult quote =
   let code : string = compile quote
   try
      let engine = 
        Engine()
            .SetValue("test_log", System.Action<string>(printfn "//[LOG] %s"))
            .SetValue("setTimeout", System.Func<System.Action, double, int>(fun f _ -> f.Invoke(); 0))
            .SetValue("setInterval", System.Func<System.Action, double, int>(fun f _ -> f.Invoke(); 0))
            .SetValue("clearTimeout", System.Action<int>(fun _ -> ()))
            .SetValue("clearInterval", System.Action<int>(fun _ -> ()))
      let result = engine.Execute(prerequisiteJS + System.Environment.NewLine + code).GetCompletionValue().ToObject()
      let message (ex: 'a) (re: 'b) = sprintf "%sExpected: %A%sBut was: %A" System.Environment.NewLine ex System.Environment.NewLine re
      Assert.That((result = expectedResult), (message expectedResult result))
   // Wrap xUnit exceptions to stop pauses.
   with ex ->
      printfn "// Code:\n%s" code
      if ex.GetType().Namespace.StartsWith "FunScript" then raise ex
      else failwithf "Message: %s\n" ex.Message

let checkAreEqualWithComponents components expectedResult quote = 
    checkAreEqualWith "" (compileWithComponents components) expectedResult quote
    
let checkAreEqual expectedResult quote =
    checkAreEqualWithComponents [] expectedResult quote

/// Bootstrapping:
/// Generates code. Runs it through a JS interpreter. 
/// Checks the result against the compiled expression.
let check (quote:Quotations.Expr) =
   let expectedResult = quote.EvalUntyped()
   checkAreEqual expectedResult quote

let rxLib =
    lazy System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "../../../../lib/RxJs/rx.all.compat.js")

let checkRx (quote:Quotations.Expr) =
   let expectedResult = quote.EvalUntyped()
   checkAreEqualWith rxLib.Value compileWithRx expectedResult quote

let checkAsync (quote:Quotations.Expr<'a Async>) =
    let expectedResult = <@ Async.RunSynchronously %quote @>.Eval()
    let immediateQuote = 
        <@ 
            let result = ref None
            async { 
                let! v = %quote 
                result := Some v
            } |> Async.StartImmediate
            !result |> Option.get 
        @>
    checkAreEqual expectedResult immediateQuote