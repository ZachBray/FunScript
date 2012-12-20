#if INTERACTIVE
#load "Interactive.fsx"
open FunJS.Tests.Common
#endif
[<NUnit.Framework.TestFixture>] 
module FunJS.Tests.Asyncs

open NUnit.Framework


[<Test>]
let ``Simple async translates without exception``() =
   check 
      <@@ 
         async { return () }
         |> Async.StartImmediate
      @@>
