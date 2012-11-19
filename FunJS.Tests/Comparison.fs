module FunJS.Tests.Comparison

open Xunit
open FunJS

[<Fact>]
let ``Infix equality works``() =
   check <@@ 2. = 3. @@>

[<Fact>]
let ``Infix inequality works``() =
   check <@@ 2. <> 3. @@>

[<Fact>]
let ``Infix less than works``() =
   check <@@ 2. < 3. @@>

[<Fact>]
let ``Infix greater than works``() =
   check <@@ 2. > 3. @@>

[<Fact>]
let ``Infix less than or equal to works``() =
   check <@@ 2. <= 3. @@>

[<Fact>]
let ``Infix greater than or equal to works``() =
   check <@@ 2. >= 3. @@>


[<JS>]
type Address = { PostCode: int }

[<JS>] //Note: Jint doesn't compare strings correctly.
type Person = { IsMale: bool; Age: int; Address: Address }

[<Fact>]
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
               { IsMale = isMaleA
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { IsMale = isMaleB
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA = personB
         @@>

[<Fact>]
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
               { IsMale = isMaleA
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { IsMale = isMaleB
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA <> personB
         @@>

[<Fact>]
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
               { IsMale = isMaleA
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { IsMale = isMaleB
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA < personB
         @@>

[<Fact>]
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
               { IsMale = isMaleA
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { IsMale = isMaleB
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA > personB
         @@>

[<Fact>]
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
               { IsMale = isMaleA
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { IsMale = isMaleB
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA <= personB
         @@>

[<Fact>]
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
               { IsMale = isMaleA
                 Age = ageA
                 Address = { PostCode = addressA } }
            let personB = 
               { IsMale = isMaleB
                 Age = ageB
                 Address = { PostCode = addressB } }
            personA >= personB
         @@>