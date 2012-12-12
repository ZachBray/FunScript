#if INTERACTIVE
#load "Interactive.fsx"
open FunJS.Tests.Common
#endif
module FunJS.Tests.Strings

open Xunit
open Xunit.Extensions
open FsUnit.Xunit

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