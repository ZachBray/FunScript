#if INTERACTIVE
#load "Interactive.fsx"
open FunJS.Tests.Common
#endif
module FunJS.Tests.Asyncs

open Xunit
open Xunit.Extensions
open FsUnit.Xunit

[<Fact>]
let ``Simple async translates without exception``() =
   check 
      <@@ 
         async { return () }
         |> Async.StartImmediate
      @@>
