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
