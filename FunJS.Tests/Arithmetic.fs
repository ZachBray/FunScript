module FunJS.Tests.Arithmetic

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Infix add can be generated``() =
   check <@@ 4. + 2. @@>

[<Fact>]
let ``Infix subtract can be generated``() =
   check <@@ 4. - 2. @@>

[<Fact>]
let ``Infix multiply can be generated``() =
   check <@@ 4. * 2. @@>

[<Fact>]
let ``Infix divide can be generated``() =
   check <@@ 4. / 2. @@>

[<Fact>]
let ``Infix modulo can be generated``() =
   check <@@ 4. % 2. @@>

[<Fact>]
let ``Evaluation order is preserved by generated code``() =
   check <@@ (4. - 2.) * 2. + 1. @@>

