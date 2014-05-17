[<NUnit.Framework.TestFixture>]
module FunScript.Tests.Logic

open NUnit.Framework


[<Test>]
let ``Infix AND can be generated``() =
   check <@@ true && false @@>

[<Test>]
let ``Infix OR can be generated``() =
   check <@@ true && false @@>

[<Test>]
let ``Not can be generated``() =
   check <@@ not false @@>

[<Test>]
let ``Evaluation order is preserved by generated code``() =
   check <@@ (true && false) || false  @@>

[<Test>]
let ``AND is a short circuiting operator``() =
   check 
      <@@
         let x = ref false
         let f() =
            x := true 
            true
         !x && f()
      @@>

[<Test>]
let ``OR is a short circuiting operator``() =
   check 
      <@@
         let x = ref true
         let f() =
            x := false 
            false
         !x || f()
      @@>
