#if INTERACTIVE
#load "Interactive.fsx"
open FunScript.Tests.Common
#endif
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Asyncs

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
