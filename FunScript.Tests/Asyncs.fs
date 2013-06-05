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
