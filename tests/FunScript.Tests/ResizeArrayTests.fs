[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.ResizeArrays

open NUnit.Framework

[<Test>]
let ``ResizeArray zero creation works``() =
   check 
      <@@ 
         let li = ResizeArray<float>()
         true
      @@>

[<Test>]
let ``ResizeArray zero creation with size works``() =
   check 
      <@@ 
         let li = ResizeArray<string>(5)
         true
      @@>

[<Test>]
let ``ResizeArray creation with seq works``() =
   check 
      <@@ 
         let li = ResizeArray<_>(seq{1. .. 5.})
         li.[2]
      @@>

[<Test>]
let ``ResizeArray casting to seq works``() =
   check 
      <@@ 
         let xs = ResizeArray<_>(seq{1. .. 5.}) :> seq<_>
         Seq.sum xs
      @@>

[<Test>]
let ``ResizeArray iteration works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
         let acc = ref 0.
         for i in li do
            acc := !acc + i
         !acc
      @@>

[<Test>]
let ``ResizeArray iteration with index works``() =
   checkAreEqual
      10. 
      <@@
         let li = ResizeArray<_>()
         for i = 1 to 4 do
            li.Add(float i)
         let mutable x = 0.
         for i = 0 to li.Count - 1 do
            x <- x + li.[i]
         x
      @@>

[<Test>]
let ``ResizeArray folding works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
         li |> Seq.fold (fun acc item -> acc + item) 0.
      @@>

[<Test>]
let ``ResizeArray.Count works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
         li.Count |> float
      @@>

[<Test>]
let ``ResizeArray indexer getter works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
         li.[1]
      @@>

[<Test>]
let ``ResizeArray indexer setter works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
         li.[3] <- 10.
         li.[3]
      @@>

[<Test>]
let ``ResizeArray.Clear works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
         li.Clear()
         li.Count |> float
      @@>

[<Test>]
let ``ResizeArray.Add works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
         li.Add(6.)
         li.Count |> float
      @@>

[<Test>]
let ``ResizeArray.AddRange works``() =
   check
      <@@ 
         let li = ResizeArray<_>()
         li.AddRange [1;2;3]
         li.Count |> float
      @@>

[<TestCase("ab"); TestCase("cd")>]
let ``ResizeArray.Contains works``(str:string) =
   check  
      <@@
         let li = ResizeArray<_>()
         li.Add("ab")
         li.Add("ch")
         li.Contains(str)
      @@>

[<TestCase("ab"); TestCase("cd")>]
let ``ResizeArray.IndexOf works``(str:string) =
   check  
      <@@
         let li = ResizeArray<_>()
         li.Add("ch")
         li.Add("ab")
         li.IndexOf(str) |> float
      @@>

[<TestCase("ab"); TestCase("cd")>]
let ``ResizeArray.Remove works``(str:string) =
   check  
      <@@
         let li = ResizeArray<_>()
         li.Add("ab")
         li.Add("ch")
         li.Remove(str)
      @@>

[<Test>]
let ``ResizeArray.RemoveAt works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
         li.RemoveAt(2)
         li.[2]
      @@>

[<Test>]
let ``ResizeArray.Insert works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
         li.Insert(2, 8.)
         li.[2]
      @@>

[<Test>]
let ``ResizeArray.ReverseInPlace works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
         li.Reverse()
         li.[2]
      @@>

[<Test>]
let ``ResizeArray.SortInPlace works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add("Ana"); li.Add("Pedro"); li.Add("Lucía"); li.Add("Paco")
         li.Sort()
         li.[2]
      @@>

[<Test>]
let ``ResizeArray.SortInPlaceWith works``() =
   check 
      <@@ 
         let li = ResizeArray<_>()
         li.Add(3.); li.Add(6.); li.Add(5.); li.Add(4.); li.Add(8.) 
         li.Sort(fun x y -> if x > y then -1 elif x < y then 1 else 0)
         li.Sort()
         li.[3]
      @@>


