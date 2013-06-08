[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.CommonOperators

open NUnit.Framework

open FunScript

[<Test>]
let ``the id function works``() =
   check  
      <@@ 
         id 10.
      @@>

[<Test>]
let ``the ref function works``() =
   check  
      <@@ 
         let x = ref 10.
         ()
      @@>


[<Test>]
let ``the ! operator works``() =
   check  
      <@@ 
         let x = ref 10.
         !x
      @@>

[<Test>]
let ``the := operator works``() =
   check  
      <@@ 
         let x = ref 10.
         x := 11.
         !x
      @@>


[<Test>]
let ``the |> operator works``() =
   check  
      <@@ 
         let incr x = x + 1.
         let x = 10.
         x |> incr
      @@>

[<Test>]
let ``the |> operator works with functions that return functions``() =
   check  
      <@@ 
         let add x = fun (y, z) -> x + y + z
         let addPartial = 9. |> add
         addPartial (10., 11.)
      @@>

[<Test>]
let ``the |> operator works with functions that return functions that return functions``() =
   check  
      <@@ 
         let add x = 
            fun (y, z) -> 
               let r = x + y + z
               fun k ->
                  r + k
         let addPartial = 9. |> add
         let addPartial = addPartial (10., 11.)
         addPartial 12.
      @@>

[<Test>]
let ``the ||> operator works``() =
   check  
      <@@ 
         let add x y = x + y
         let arg = 10., 11.
         arg ||> add
      @@>

[<Test>]
let ``the |||> operator works``() =
   check  
      <@@ 
         let add x y z = x + y + z
         let arg = 10., 11., 12.
         arg |||> add
      @@>

[<Test>]
let ``the <| operator works``() =
   check  
      <@@ 
         let incr x = x + 1.
         let x = 10.
         incr <| x
      @@>

[<Test>]
let ``the <|| operator works``() =
   check  
      <@@ 
         let add x y = x + y
         let arg = 10., 11.
         add <|| arg
      @@>

[<Test>]
let ``the <||| operator works``() =
   check  
      <@@ 
         let add x y z = x + y + z
         let arg = 10., 11., 12.
         add <||| arg
      @@>

[<Test>]
let ``the >> operator works``() =
   check  
      <@@ 
         let incr x = x + 1.
         let divBy2 x = x / 2.
         (incr >> divBy2) 10.
      @@>

[<Test>]
let ``the << operator works``() =
   check  
      <@@ 
         let incr x = x + 1.
         let divBy2 x = x / 2.
         (incr << divBy2) 10.
      @@>

[<Test>]
let ``the >> operator works with functions that take tuples``() =
   check  
      <@@ 
         let incr (x, y) = x + 1., y + 1.
         let divBy2 (x, y) = x / 2., y /2.
         let result = (incr >> divBy2) (10., 100.)
         fst result
      @@>


[<Test>]
let ``the defaultArg function works``() =
   check  
      <@@ 
         let x: float option = None
         defaultArg x 10.
      @@>

[<Test>]
let ``the ignore function works``() =
   check  
      <@@ 
         ignore 11.
      @@>

[<Test>]
let ``the ignore function evaluates its arguments``() =
   check  
      <@@ 
         let wasEvaluated = ref false
         ignore ((fun () -> wasEvaluated := true)())
         !wasEvaluated
      @@>

[<Test>]
let ``the .. operator works``() =
   check
      <@@
         let xs = [10. .. 20.]
         xs.[0] + xs.[1]
       @@>

[<Test>]
let ``the .. .. operator works``() =
   check
      <@@
         let xs = [10. .. 2. .. 20.]
         xs.[0] + xs.[1]
       @@>