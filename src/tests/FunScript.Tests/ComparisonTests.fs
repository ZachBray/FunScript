[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Comparison

open NUnit.Framework
open FunScript

[<Test>]
let ``Infix equality works``() =
   check <@@ 2. = 3. @@>

[<Test>]
let ``obj.ReferenceEquals works with null``() =
   check 
       <@@ 
           let x : obj = null
           let y : obj = null
           obj.ReferenceEquals(x, y)
       @@>

[<Test>]
let ``obj.ReferenceEquals works with objs``() =
   check 
       <@@ 
           let x = Some 1
           let y = x
           obj.ReferenceEquals(x, y)
       @@>

[<Test>]
let ``obj.ReferenceEquals works with objs & null``() =
   check 
       <@@ 
           let x = Some 1
           let y : obj = null
           obj.ReferenceEquals(x, y)
       @@>

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

let forVariousTuplesCheck f =
    for x in [0; 1] do
    for y in [0; 1] do
    for r in [0; 1] do
    for s in [0; 1] do
        check 
            <@@
                let a = x, y
                let b = r, s
                (%f) a b
            @@>

[<Test>]
let ``tuple equality works``() =
   forVariousTuplesCheck <@ (=) @>

[<Test>]
let ``tuple inequality works``() =
   forVariousTuplesCheck <@ (<>) @>

[<Test>]
let ``tuple greater than works``() =
   forVariousTuplesCheck <@ (>) @>

[<Test>]
let ``tuple greater than or equal works``() =
   forVariousTuplesCheck <@ (>=) @>

[<Test>]
let ``tuple less than works``() =
   forVariousTuplesCheck <@ (<) @>

[<Test>]
let ``tuple less than or equal works``() =
   forVariousTuplesCheck <@ (<=) @>

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

let forVariousListsCheck f =
    for x in [0; 1] do
    for y in [0; 1] do
    for r in [0; 1] do
    for s in [0; 1] do
        check 
            <@@
                let a = [x; y]
                let b = [r; s]
                (%f) a b
            @@>

[<Test>]
let ``list equality works``() =
   forVariousListsCheck <@ (=) @>

[<Test>]
let ``list inequality works``() =
   forVariousListsCheck <@ (<>) @>

[<Test>]
let ``list greater than works``() =
   forVariousListsCheck <@ (>) @>

[<Test>]
let ``list greater than or equal works``() =
   forVariousListsCheck <@ (>=) @>

[<Test>]
let ``list less than works``() =
   forVariousListsCheck <@ (<) @>

[<Test>]
let ``list less than or equal works``() =
   forVariousListsCheck <@ (<=) @>

let forVariousMapsCheck f =
    for x in [0; 1] do
    for y in [0; 1] do
    for r in [0; 1] do
    for s in [0; 1] do
        check 
            <@@
                let a = Map.empty |> Map.add x y
                let b = Map.empty |> Map.add r s
                (%f) a b
            @@>

[<Test>]
let ``map equality works``() =
   forVariousMapsCheck <@ (=) @>

[<Test>]
let ``map inequality works``() =
   forVariousMapsCheck <@ (<>) @>

[<Test>]
let ``map greater than works``() =
   forVariousMapsCheck <@ (>) @>

[<Test>]
let ``map greater than or equal works``() =
   forVariousMapsCheck <@ (>=) @>

[<Test>]
let ``map less than works``() =
   forVariousMapsCheck <@ (<) @>

[<Test>]
let ``map less than or equal works``() =
   forVariousMapsCheck <@ (<=) @>

let forVariousSetsCheck f =
    for x in [0; 1] do
    for y in [0; 1] do
    for r in [0; 1] do
    for s in [0; 1] do
        check 
            <@@
                let a = Set.empty |> Set.add x |> Set.add y
                let b = Set.empty |> Set.add r |> Set.add s
                (%f) a b
            @@>

[<Test>]
let ``set equality works``() =
   forVariousSetsCheck <@ (=) @>

[<Test>]
let ``set inequality works``() =
   forVariousSetsCheck <@ (<>) @>

[<Test>]
let ``set greater than works``() =
   forVariousSetsCheck <@ (>) @>

[<Test>]
let ``set greater than or equal works``() =
   forVariousSetsCheck <@ (>=) @>

[<Test>]
let ``set less than works``() =
   forVariousSetsCheck <@ (<) @>

[<Test>]
let ``set less than or equal works``() =
   forVariousSetsCheck <@ (<=) @>

[<JS>]
type CustomComparable<'a, 'b when 'a : comparison and 'b : comparison>(x:'a, y:'b) =
    member val private X = x
    member val private Y = y
    override this.Equals obj = 
        let that = obj :?> CustomComparable<'a, 'b>
        compare this that = 0
    override __.GetHashCode() = failwith "Not implemented"
    interface System.IComparable with
        member this.CompareTo(obj) =
            let that = obj :?> CustomComparable<'a, 'b>
            let diff = compare this.X that.X
            if diff <> 0 then diff
            else compare this.Y that.Y

let forVariousCustomComparablesCheck f =
    for x in [0; 1] do
    for y in ["0"; "1"] do
    for r in [0; 1] do
    for s in ["0"; "1"] do
        check 
            <@@
                let a = CustomComparable(x, y)
                let b = CustomComparable(r, s)
                (%f) a b
            @@>

[<Test>]
let ``custom IComparable implementation equality works``() =
   forVariousCustomComparablesCheck <@ (=) @>

[<Test>]
let ``custom IComparable implementation inequality works``() =
   forVariousCustomComparablesCheck <@ (<>) @>

[<Test>]
let ``custom IComparable implementation greater than works``() =
   forVariousCustomComparablesCheck <@ (>) @>

[<Test>]
let ``custom IComparable implementation greater than or equal works``() =
   forVariousCustomComparablesCheck <@ (>=) @>

[<Test>]
let ``custom IComparable implementation less than works``() =
   forVariousCustomComparablesCheck <@ (<) @>

[<Test>]
let ``custom IComparable implementation less than or equal works``() =
   forVariousCustomComparablesCheck <@ (<=) @>


[<JS; CustomComparison; CustomEquality>]
type CustomComparableRecord<'a, 'b when 'a : comparison and 'b : comparison> = 
    { X : 'a; Y : 'b }
    override this.Equals obj = 
        let that = obj :?> CustomComparableRecord<'a, 'b>
        compare this that = 0
    override __.GetHashCode() = failwith "Not implemented"
    interface System.IComparable with
        member this.CompareTo(obj) =
            let that = obj :?> CustomComparableRecord<'a, 'b>
            let diff = compare this.X that.X
            if diff <> 0 then diff
            else compare this.Y that.Y
            

let forVariousCustomRecordComparablesCheck f =
    for x in [0; 1] do
    for y in ["0"; "1"] do
    for r in [0; 1] do
    for s in ["0"; "1"] do
        check 
            <@@
                let a = CustomComparable(x, y)
                let b = CustomComparable(r, s)
                (%f) a b
            @@>

[<Test>]
let ``custom IComparable record implementation equality works``() =
   forVariousCustomComparablesCheck <@ (=) @>

[<Test>]
let ``custom IComparable record implementation inequality works``() =
   forVariousCustomRecordComparablesCheck <@ (<>) @>

[<Test>]
let ``custom IComparable record implementation greater than works``() =
   forVariousCustomRecordComparablesCheck <@ (>) @>

[<Test>]
let ``custom IComparable record implementation greater than or equal works``() =
   forVariousCustomRecordComparablesCheck <@ (>=) @>

[<Test>]
let ``custom IComparable record implementation less than works``() =
   forVariousCustomRecordComparablesCheck <@ (<) @>

[<Test>]
let ``custom IComparable record implementation less than or equal works``() =
   forVariousCustomRecordComparablesCheck <@ (<=) @>

[<Test>]
let ``unit equality works``() =
    check
        <@@
            let a = ()
            let b = ()
            a = b
        @@>

[<Test>]
let ``unit less than works``() =
    check
        <@@
            let a = ()
            let b = ()
            a < b
        @@>
