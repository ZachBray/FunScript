#if INTERACTIVE
#load "Interactive.fsx"
open FunJS.Tests.Common
#endif
module FunJS.Tests.Strings

open Xunit
open Xunit.Extensions
open FsUnit.Xunit

// System.String - instance methods

[<Theory>]
[<InlineData(null:string); InlineData(""); InlineData("test")>]
let ``String.IsNullOrEmpty works``(str:string) =
   check  
      <@@ 
         System.String.IsNullOrEmpty(str)
      @@>

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
let ``String.Length works``() =
   checkAreEqual 4.
      <@@ 
         "abcd".Length
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

let ``String.ToCharArray works``() =
   check 
      <@@
         let a = "abcd".ToCharArray()
         a.[0].ToString() + a.[1].ToString() + a.[2].ToString() + a.[3].ToString()
      @@>

// System.String - static methods (INCOMPLETE)

[<Theory>]
[<InlineData(null:string); InlineData(""); InlineData("test")>]
let ``String.IsNullOrEmpty works``(str:string) =
   check  
      <@@ 
         System.String.IsNullOrEmpty(str)
      @@>

// [<Fact>] - Messed up, because some overloads are generic and some are not
// (and some take arrays while others take sequences...)
let ``String.Join works``() =
   check 
      <@@ 
         System.String.Join(",", [| "a"; "b"; "c" |])
      @@>


// String - F# module functions 

// [<Theory>]
// [<InlineData("!!!"); InlineData("a!a"); InlineData("aaa")>]
// 
// This is very odd, but if 'str = "aaa"' then Jint seems to be returning
// true (which is wrong) but Chrome JS console gives false as expected.
let ``String.forall and exists work``(str) =
   check 
      <@@ 
         // let str = "aaa"
         str |> String.forall (fun c -> c.ToString() = "!") 
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

// String.iter
// String.iteri
// String.length
// String.map
// String.mapi
// String.replicate
