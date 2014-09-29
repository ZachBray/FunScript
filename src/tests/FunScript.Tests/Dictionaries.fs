[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Dictionaries

open NUnit.Framework
open System.Collections.Generic

[<Test>]
let ``Dictionary creation works``() =
   check 
      <@@ 
         let dic = Dictionary<_,_>()
         true
      @@>

[<Test>]
let ``Dictionary creation with size works``() =
   check 
      <@@ 
         let dic = Dictionary<_,_>(5)
         true
      @@>

[<Test>]
let ``Interface IDictionary creation works``() =
   check 
      <@@ 
         let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
         dic.["2"]
      @@>

[<Test>]
let ``Dictionary creation from IDictionary works``() =
   check 
      <@@ 
         let idic = dict <| seq { for i in 1 .. 10 -> i.ToString(), i*i }
         let idicCount = idic.Count
         let dic = Dictionary<_,_>(idic)
         dic.Add("100", 100)
         System.String.Format("IDictionary Count: {0} - Dictionary Count: {1}", idicCount, dic.Count).ToLower()
      @@>

[<Test>]
let ``Interface IDictionary iteration works``() =
   check 
      <@@ 
         let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
         let i = ref 0.
         for kv in dic do
            i := kv.Value + !i
         !i
      @@>

[<Test>]
let ``Interface IDictionary folding works``() =
   check 
      <@@ 
         let dic = dict [ ("A", 1.); ("B", 2.); ("C", 3.) ]
         dic |> Seq.fold (fun acc item -> acc + item.Value) 0.
      @@>

[<Test>]
let ``Interface IDictionary.IsReadOnly works``() =
   check 
      <@@ 
         let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
         dic.IsReadOnly
      @@>

[<Test>]
let ``Dictionary iteration works``() =
   check
      <@@ 
         let dic = Dictionary<_,_>()
         for i in 1. .. 10. do dic.Add(i, i*i)
         let i = ref 0.
         for kv in dic do
            i := kv.Value + !i
         !i + dic.[1.]
      @@>

[<Test>]
let ``Dictionary folding works``() =
   check 
      <@@ 
         let dic = Dictionary<_,_>()
         for i in 1. .. 10. do dic.Add(i, i*i)
         dic |> Seq.fold (fun acc item -> acc + item.Value) 0.
      @@>

[<Test>]
let ``Dictionary.Count works``() =
   check 
      <@@ 
         let dic = Dictionary<_,_>()
         for i in 1. .. 10. do dic.Add(i, i*i)
         dic.Count |> float
      @@>

[<Test>]
let ``Dictionary indexer getter works``() =
   check 
      <@@
        let dic = Dictionary<string,obj>()
        dic.Add("A", "Hello")
        dic.Add("B", 2)
        dic.["B"].ToString()
      @@>

[<Test>]
let ``Dictionary.Keys works``() =
   check 
      <@@ 
         let dic = Dictionary<_,_>()
         dic.Add("A", 1)
         dic.Add("B", 2)
         dic.Keys |> Seq.fold (fun acc k -> acc + dic.[k]) 0 |> float
      @@>

[<Test>]
let ``Dictionary.Values works``() =
   check 
      <@@ 
         let dic = Dictionary<_,_>()
         dic.Add("A", 1)
         dic.Add("B", 2)
         let i = ref 0
         for value in dic.Values do
            i := value + !i
         !i |> float
      @@>

[<Test>]
let ``Dictionary.Clear works``() =
   check 
      <@@ 
         let dic = Dictionary<_,_>()
         dic.Add("A", 1)
         dic.Add("B", 2)
         dic.Clear()
         dic.Count |> float
      @@>

[<Test>]
let ``Dictionary.Add works``() =
   check 
      <@@ 
        let dic = Dictionary<_,_>()
        dic.Add("A", "Hello")
        dic.Add("B", "World!")
        dic.Count |> float
      @@>


[<TestCase("A"); TestCase("C")>]
let ``Dictionary.ContainsKey works``(str:string) =
   check  
      <@@
        let dic = Dictionary<_,_>()
        dic.Add("A", "Hello")
        dic.Add("B", "World!")
        dic.ContainsKey(str)
      @@>

[<TestCase("Hello"); TestCase("Everybody!")>]
let ``Dictionary.ContainsValue works``(str:string) =
   check  
      <@@
        let dic = Dictionary<_,_>()
        dic.Add("A", "Hello")
        dic.Add("B", "World!")
        dic.ContainsValue(str)
      @@>

[<TestCase("A"); TestCase("C")>]
let ``Dictionary.Remove works``(str:string) =
   check  
      <@@ 
        let dic = Dictionary<_,_>()
        dic.Add("A", "Hello")
        dic.Add("B", "World!")
        let removed = dic.Remove(str)
        System.String.Format("Removed? {0} Count {1}", removed, dic.Count).ToLower()
      @@>

[<Test>]
let ``Interface IDictionary.Count works``() =
   check 
      <@@ 
         let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
         dic.Count |> float
      @@>

[<Test>]
let ``Interface IDictionary indexer getter works``() =
   check 
      <@@
         let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
         dic.["2"]
      @@>

[<Test>]
let ``Interface IDictionary.Keys works``() =
   check 
      <@@ 
         let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
         let i = ref ""
         for key in dic.Keys do
            i := key + !i
         !i
      @@>

[<Test>]
let ``Interface IDictionary.Values works``() =
   check 
      <@@ 
         let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
         let i = ref 0.
         for value in dic.Values do
            i := value + !i
         !i
      @@>

[<TestCase("A"); TestCase("C")>]
let ``Interface IDictionary.ContainsKey works``(str:string) =
   check  
      <@@
         let dic = dict <| [ ("A", 1.); ("B", 2.) ]
         dic.ContainsKey(str)
      @@>

