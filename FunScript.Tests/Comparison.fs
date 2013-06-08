[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Comparison

open NUnit.Framework
open FunScript

[<Test>]
let ``Infix equality works``() =
   check <@@ 2. = 3. @@>

[<Test>]
let ``Infix inequality works``() =
   check <@@ 2. <> 3. @@>

[<Test>]
let ``Infix less than works``() =
   check <@@ 2. < 3. @@>

[<Test>]
let ``Infix greater than works``() =
   check <@@ 2. > 3. @@>

[<Test>]
let ``Infix less than or equal to works``() =
   check <@@ 2. <= 3. @@>

[<Test>]
let ``Infix greater than or equal to works``() =
   check <@@ 2. >= 3. @@>


[<JS>]
type Address = { PostCode: int }

// Becuase Enums are not supported yet...
[<JS>]
module GenderType = 
   let Male = 0 
   let Female = 1

// Doesn't work in Jint because of Tag (string) comparison
//     type Gender = Male | Female
// but this does:
type Gender = Gender of int


[<JS>] //Note: Jint doesn't compare strings correctly.
type Person = { Gender: Gender; Age: int; Address: Address }

[<Test>]
let ``structural equality works``() =
   for isMaleA in [true; false] do
   for ageA in [0; 1] do
   for addressA in [0; 1] do
   for isMaleB in [true; false] do
   for ageB in [0; 1] do
   for addressB in [0; 1] do
      check
         <@@
            let personA = 
               { Gender = Gender(if isMaleA then GenderType.Male else GenderType.Female)
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { Gender = Gender(if isMaleB then GenderType.Male else GenderType.Female)
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA = personB
         @@>

[<Test>]
let ``structural inequality works``() =
   for isMaleA in [true; false] do
   for ageA in [0; 1] do
   for addressA in [0; 1] do
   for isMaleB in [true; false] do
   for ageB in [0; 1] do
   for addressB in [0; 1] do
      check
         <@@
            let personA = 
               { Gender = Gender(if isMaleA then GenderType.Male else GenderType.Female)
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { Gender = Gender(if isMaleB then GenderType.Male else GenderType.Female)
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA <> personB
         @@>

[<Test>]
let ``structural less than works``() =
   for isMaleA in [true; false] do
   for ageA in [0; 1] do
   for addressA in [0; 1] do
   for isMaleB in [true; false] do
   for ageB in [0; 1] do
   for addressB in [0; 1] do
      check
         <@@
            let personA = 
               { Gender = Gender(if isMaleA then GenderType.Male else GenderType.Female)
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { Gender = Gender(if isMaleB then GenderType.Male else GenderType.Female)
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA < personB
         @@>

[<Test>]
let ``structural greater than works``() =
   for isMaleA in [true; false] do
   for ageA in [0; 1] do
   for addressA in [0; 1] do
   for isMaleB in [true; false] do
   for ageB in [0; 1] do
   for addressB in [0; 1] do
      check
         <@@
            let personA = 
               { Gender = Gender(if isMaleA then GenderType.Male else GenderType.Female)
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { Gender = Gender(if isMaleB then GenderType.Male else GenderType.Female)
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA > personB
         @@>

[<Test>]
let ``structural less than or equal to works``() =
   for isMaleA in [true; false] do
   for ageA in [0; 1] do
   for addressA in [0; 1] do
   for isMaleB in [true; false] do
   for ageB in [0; 1] do
   for addressB in [0; 1] do
      check
         <@@
            let personA = 
               { Gender = Gender(if isMaleA then GenderType.Male else GenderType.Female)
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { Gender = Gender(if isMaleB then GenderType.Male else GenderType.Female)
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA <= personB
         @@>

[<Test>]
let ``structural greater than or equal to works``() =
   for isMaleA in [true; false] do
   for ageA in [0; 1] do
   for addressA in [0; 1] do
   for isMaleB in [true; false] do
   for ageB in [0; 1] do
   for addressB in [0; 1] do
      check
         <@@
            let personA = 
               { Gender = Gender(if isMaleA then GenderType.Male else GenderType.Female)
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { Gender = Gender(if isMaleB then GenderType.Male else GenderType.Female)
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA >= personB
         @@>


[<Test>]
let ``tuple equality works``() =
   for x in [0; 1] do
   for y in [0; 1] do
   for r in [0; 1] do
   for s in [0; 1] do
      check
         <@@
            let a = x, y
            let b = r, s
            a = b
         @@>

[<Test>]
let ``tuple inequality works``() =
   for x in [0; 1] do
   for y in [0; 1] do
   for r in [0; 1] do
   for s in [0; 1] do
      check
         <@@
            let a = x, y
            let b = r, s
            a <> b
         @@>

[<Test>]
let ``tuple greater than works``() =
   for x in [0; 1] do
   for y in [0; 1] do
   for r in [0; 1] do
   for s in [0; 1] do
      check
         <@@
            let a = x, y
            let b = r, s
            a > b
         @@>

[<Test>]
let ``tuple greater than or equal works``() =
   for x in [0; 1] do
   for y in [0; 1] do
   for r in [0; 1] do
   for s in [0; 1] do
      check
         <@@
            let a = x, y
            let b = r, s
            a > b
         @@>

[<Test>]
let ``tuple less than works``() =
   for x in [0; 1] do
   for y in [0; 1] do
   for r in [0; 1] do
   for s in [0; 1] do
      check
         <@@
            let a = x, y
            let b = r, s
            a < b
         @@>

[<Test>]
let ``tuple less than or equal works``() =
   for x in [0; 1] do
   for y in [0; 1] do
   for r in [0; 1] do
   for s in [0; 1] do
      check
         <@@
            let a = x, y
            let b = r, s
            a <= b
         @@>

type 'a RoseTree = RoseTree of 'a * 'a RoseTree []

[<Test>]
let ``recursive type equality works``() =
      for x in [0; 1] do
      for y in [0; 1] do
         check
            <@@
               let a = RoseTree(1, [| RoseTree(2, [||]); RoseTree(x, [||]) |])
               let b = RoseTree(1, [| RoseTree(2, [||]); RoseTree(y, [||]) |])
               a = b
            @@>

[<Test>]
let ``recursive type inequality works``() =
      for x in [0; 1] do
      for y in [0; 1] do
         check
            <@@
               let a = RoseTree(1, [| RoseTree(2, [||]); RoseTree(x, [||]) |])
               let b = RoseTree(1, [| RoseTree(2, [||]); RoseTree(y, [||]) |])
               a <> b
            @@>

[<Test>]
let ``recursive type greater than works``() =
      for x in [0; 1] do
      for y in [0; 1] do
         check
            <@@
               let a = RoseTree(1, [| RoseTree(2, [||]); RoseTree(x, [||]) |])
               let b = RoseTree(1, [| RoseTree(2, [||]); RoseTree(y, [||]) |])
               a > b
            @@>

[<Test>]
let ``recursive type greater than or equal works``() =
      for x in [0; 1] do
      for y in [0; 1] do
         check
            <@@
               let a = RoseTree(1, [| RoseTree(2, [||]); RoseTree(x, [||]) |])
               let b = RoseTree(1, [| RoseTree(2, [||]); RoseTree(y, [||]) |])
               a >= b
            @@>

[<Test>]
let ``recursive type less than works``() =
      for x in [0; 1] do
      for y in [0; 1] do
         check
            <@@
               let a = RoseTree(1, [| RoseTree(2, [||]); RoseTree(x, [||]) |])
               let b = RoseTree(1, [| RoseTree(2, [||]); RoseTree(y, [||]) |])
               a < b
            @@>

[<Test>]
let ``recursive type less than or equal works``() =
      for x in [0; 1] do
      for y in [0; 1] do
         check
            <@@
               let a = RoseTree(1, [| RoseTree(2, [||]); RoseTree(x, [||]) |])
               let b = RoseTree(1, [| RoseTree(2, [||]); RoseTree(y, [||]) |])
               a <= b
            @@>