#if INTERACTIVE
#load "Interactive.fsx"
open FunScript.Tests.Common
#endif
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Asyncs

open System
open NUnit.Framework


[<Test>]
let ``Simple async translates without exception``() =
   check 
      <@@ 
         async { return () }
         |> Async.StartImmediate
      @@>

[<Test>]
let ``Async while binding works correctly``() =
   checkAreEqual 10.0
      <@@ 
        let result = ref 0.0
        async { 
            while !result < 10.0 do
                result := !result + 1.0
        } |> Async.StartImmediate
        !result
      @@>

[<Test>]
let ``Async for binding works correctly``() =
   checkAreEqual 6.0
      <@@ 
        let inputs = [|1.0; 2.0; 3.0|]
        let result = ref 0.0
        async { 
            for inp in inputs do
                result := !result + inp
        } |> Async.StartImmediate
        !result
      @@>

[<Test>]
let ``Async exceptions are handled correctly``() =
   checkAreEqual 22.0
      <@@ 
         let result = ref 0.0
         let f shouldThrow =
             async { 
                try
                    if shouldThrow then failwith "boom!"
                    else result := 12.0
                with _ -> result := 10.0
             } |> Async.StartImmediate
             !result
         f true + f false
      @@>

[<Test>]
let ``Simple async is executed correctly``() =
   checkAreEqual true
      <@@ 
         let result = ref false
         let x = async { return 99 }
         async { 
            let! x = x
            let y = 99
            result := x = y
         }
         //TODO: RunSynchronously would make more sense here but in JS I think this will be ok.
         |> Async.StartImmediate 
         !result
      @@>

[<FunScript.JS>]
type DisposableAction(f) =
    interface IDisposable with
        member __.Dispose() = f()

[<Test>]
let ``async use statements should dispose of resources when the go out of scope``() =
   checkAreEqual true
      <@@ 
         let isDisposed = ref false
         let step1ok = ref false
         let step2ok = ref false
         let resource = async { return new DisposableAction(fun () -> isDisposed := true) }
         async { 
            use! r = resource
            step1ok := not !isDisposed
         }
         //TODO: RunSynchronously would make more sense here but in JS I think this will be ok.
         |> Async.StartImmediate 
         step2ok := !isDisposed
         !step1ok && !step2ok
      @@>


[<Test>]
let ``Try ... with ... expressions inside async expressions work the same``() =
    checkAsync
        <@ 
            let result = ref ""
            let throw() : unit =
                raise(exn "Boo!")
            let append(x) = 
                result := !result + x
            let innerAsync() =
                async {
                    append "b"
                    try append "c"
                        throw()
                        append "1"
                    with _ -> append "d"
                    append "e"
                }
            async { 
                append "a"
                try do! innerAsync()
                with _ -> append "2"
                append "f"
                return !result
            }
      @>