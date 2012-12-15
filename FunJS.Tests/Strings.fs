#if INTERACTIVE
#load "Interactive.fsx"
open FunJS.Tests.Common
#endif
module FunJS.Tests.Strings

open Xunit
open Xunit.Extensions
open FsUnit.Xunit

// The code that is produced by writing (c:char) = 'x' does not work correctly
// in Jint, but works fine in web browsers. This function allows us to write
// charToInt c = charToInt 'x' in the unit tests (which works in Jint too.)
[<FunJS.JS; FunJS.JSEmit("return {0}.charCodeAt(0);")>]
let charToInt (c:char) : int = int c 

// System.String - static methods

[<Theory>]
[<InlineData(null:string); InlineData(""); InlineData("test")>]
let ``String.IsNullOrEmpty works``(str:string) =
   check  
      <@@ 
         System.String.IsNullOrEmpty(str)
      @@>

// System.String - instance methods

[<Fact>]
let ``String.Split works``() =
   check 
      <@@ 
         let array = "a;b,c".Split(',', ';')
         "abc" = array.[0] + array.[1] + array.[2]
      @@>

[<Fact>]
let ``String.Replace works``() =
   check 
      <@@ 
         "abc abc abc".Replace("abc", "d")
      @@>

[<Fact>]
let ``String.IndexOf works``() =
   check 
      <@@ 
         "abcd".IndexOf("bc") * 100 + "abcd".IndexOf("bd") |> float
      @@>

[<Fact>]
let ``String.ToUpper and String.ToLower work``() =
   check 
      <@@ 
         "AbC".ToUpper() + "aBc".ToLower()
      @@>

[<Fact>]
let ``String.Length works``() =
   check
      <@@ 
         "AbC".Length |> float
      @@>

[<Fact>]
let ``String item works``() =
   checkAreEqual
      "b"
      <@@ 
         "AbC".[1]
      @@>

[<Fact>]
let ``String.ToCharArray works``() =
   check 
      <@@
         let a = "abcd".ToCharArray()
         a.[0].ToString() + a.[1].ToString() + a.[2].ToString() + a.[3].ToString()
      @@>

[<Fact>]    
let ``String.ToCharArray returns array``() =
   check 
      <@@
         "abcd".ToCharArray() |> Array.map (fun _ -> 1) |> Array.sum |> float
      @@>

[<Fact>]
let ``String.Join works``() =
   check 
      <@@ 
         System.String.Join(",", [| "a"; "b"; "c" |])
      @@>


// String - F# module functions 

[<Theory>]
[<InlineData("!!!"); InlineData("a!a"); InlineData("aaa")>]
let ``String.forall and exists work``(str) =
   check 
      <@@ 
         str |> String.forall (fun c -> charToInt c = charToInt '!') 
      @@> 

[<Fact>]
let ``String.init works``() =
   check 
      <@@ 
         String.init 3 (fun i -> "a")
      @@> 

[<Fact>]
let ``String.collect works``() =
   check 
      <@@ 
         "abc" |> String.collect (fun c -> "bcd")
      @@> 

[<Fact>]
let ``String.iter works``() =
   check 
      <@@ 
         let res = ref ""
         "Hello world!" |> String.iter (fun c -> res := !res + c.ToString())
         !res
      @@> 

[<Fact>]
let ``String.iteri works``() =
   check 
      <@@ 
         let res = ref ""
         "Hello world!" |> String.iteri (fun c i -> res := !res + i.ToString() + c.ToString())
         !res
      @@> 

[<Fact>]
let ``String.length (function) works``() =
   check
      <@@ 
         "AbC" |> String.length |> float
      @@>

[<Fact>]
let ``String.map works``() =
   check 
      <@@          
         "Hello world!" |> String.map (fun c -> if charToInt c = charToInt 'H' then '_' else c)
      @@> 

[<Fact>]
let ``String.mapi works``() =
   check 
      <@@ 
         "Hello world!" |> String.mapi (fun i c -> if i = 1 || charToInt c = charToInt 'H' then '_' else c)
      @@> 

[<Fact>]
let ``String.replicate works``() =
   check 
      <@@ 
         String.replicate 10 "hi there"
      @@> 
