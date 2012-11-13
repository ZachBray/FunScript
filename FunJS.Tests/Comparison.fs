module FunJS.Tests.Comparison

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Infix equality can be generated``() =
   check <@@ 2. = 3. @@>

[<Fact>]
let ``Infix inequality can be generated``() =
   check <@@ 2. <> 3. @@>

[<Fact>]
let ``Infix less than can be generated``() =
   check <@@ 2. < 3. @@>

[<Fact>]
let ``Infix greater than can be generated``() =
   check <@@ 2. > 3. @@>

[<Fact>]
let ``Infix less than or equal to can be generated``() =
   check <@@ 2. <= 3. @@>

[<Fact>]
let ``Infix greater than or equal to can be generated``() =
   check <@@ 2. >= 3. @@>