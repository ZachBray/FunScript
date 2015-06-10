#if INTERACTIVE
#load "Interactive.fsx"
open FunScript.Tests.Common
#endif
[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Strings

open System
open NUnit.Framework

[<TestCase("B"); TestCase("Z")>]
let ``String.Contains works``(arg) =
   check 
      <@@
         "ABC".Contains(arg)
      @@>

[<Test>]
let ``String.Format with extra formatting works``() =
   check 
      <@@
         let i = 0.5466788
         let dt = DateTime(2014, 9, 26).AddMinutes(19.)
         String.Format("{0:F2} {0:P2} {1:yy/MM/dd HH:mm}", i, dt)
      @@>

[<Test>]
let ``Console.WriteLine works``() =
   check 
      <@@
         Console.WriteLine("Testing, testing...")
         true
      @@>

// The code that is produced by writing (c:char) = 'x' does not work correctly
// in Jint, but works fine in web browsers. This function allows us to write
// charToInt c = charToInt 'x' in the unit tests (which works in Jint too.)
[<FunScript.JS; FunScript.JSEmit("return {0}.charCodeAt(0);")>]
let charToInt (c:char) : int = int c 

// System.String - static methods

[<TestCase(null:string); TestCase(""); TestCase("test")>]
let ``String.IsNullOrEmpty works``(str:string) =
   check  
      <@@ 
         System.String.IsNullOrEmpty(str)
      @@>

// System.String - instance methods

[<Test>]
let ``String.Split works``() =
   check 
      <@@ 
         let array = "a;b,c".Split(',', ';')
         "abc" = array.[0] + array.[1] + array.[2]
      @@>

[<Test>]
let ``String.Split with remove empties works``() =
   check 
      <@@ 
         let array = ";,a;b,c".Split([|','; ';'|], StringSplitOptions.RemoveEmptyEntries)
         "abc" = array.[0] + array.[1] + array.[2]
      @@>

[<Test>]
let ``String.Split with strings & remove empties works``() =
   check 
      <@@ 
         let array = ";,a;b,c".Split([|","; ";"|], StringSplitOptions.RemoveEmptyEntries)
         "abc" = array.[0] + array.[1] + array.[2]
      @@>

////TODO: Move to another file.
//[<Test>]
//let ``System.Net.WebUtility.UrlEncode works``() =
//   if typeof<System.Type>.GetType().Name = "MonoType" then Assert.Ignore("Mono/MS.NET compatibility bug") else
//   check 
//      <@@ 
//         System.Net.WebUtility.UrlEncode "<abc>foo </abc>"
//      @@>
//
////TODO: Move to another file.
//[<Test>]
//let ``System.Net.WebUtility.UrlDecode works``() =
//   check 
//      <@@ 
//         System.Net.WebUtility.UrlDecode "%3cabc%3efoo+%3c%2fabc%3e"
//      @@>
//
////TODO: Move to another file.
//[<Test>]
//let ``UrlEncode >> UrlDecode works``() =
//   check 
//      <@@ 
//         System.Net.WebUtility.UrlDecode(System.Net.WebUtility.UrlEncode " a + b + c <br> ")
//      @@>

[<Test>]
let ``System.Uri.EscapeDataString works``() =
   check 
      <@@ 
         System.Uri.EscapeDataString "<abc>abc.foo+xyz%2B</abc>"
      @@>

//TODO: Move to another file.
[<Test>]
let ``System.Uri.UnescapeDataString  works``() =
   check 
      <@@ 
         System.Uri.UnescapeDataString "%3Cabc%3Eabc.foo%2Bxyz%252B%3C%2Fabc%3E"
      @@>

[<Test>]
let ``UrlEncode >> UrlDecode works``() =
   check 
      <@@ 
         System.Uri.UnescapeDataString(System.Uri.EscapeDataString " <<< ! ==== a + b + c <abc>?=abc.foo+xyz%2B</abc>?; ")
      @@>

[<Test>]
let ``String.Replace works``() =
   check 
      <@@ 
         "abc abc abc".Replace("abc", "d")
      @@>

[<Test>]
let ``String.Replace does not get stuck in endless loop``() =
   checkAreEqual "......" 
      <@@ 
         "...".Replace(".", "..")
      @@>


[<Test>]
let ``String.IndexOf works``() =
   check 
      <@@ 
         "abcd".IndexOf("bc") * 100 + "abcd".IndexOf("bd") |> float
      @@>

[<Test>] 
let ``String.IndexOf works with offset``() =
   check 
      <@@ 
         "abcdbc".IndexOf("bc", 3) |> float
      @@>

[<Test>]
let ``String.LastIndexOf works``() =
   check 
      <@@ 
         "abcdbc".LastIndexOf("bc") * 100 + "abcd".LastIndexOf("bd") |> float
      @@>

[<Test>] 
let ``String.LastIndexOf works with offset``() =
   check 
      <@@ 
         "abcdbcebc".LastIndexOf("bc", 3) |> float
      @@>

[<TestCase("ab"); TestCase("cd"); TestCase("abcdx")>]
let ``String.StartsWith works``(str:string) =
   check  
      <@@ 
         "abcd".StartsWith(str)
      @@>

[<TestCase("ab"); TestCase("cd"); TestCase("abcdx")>]
let ``String.EndsWith works``(str:string) =
   check  
      <@@ 
         "abcd".EndsWith(str)
      @@>

[<Test>]
let ``String.Trim works``() =
   check 
      <@@ 
         "   abc   ".Trim()
      @@>

[<TestCase("ASP", "ASP.NET"); TestCase(1, 2)>]
let ``String.Format works``(arg1, arg2) =
   check 
      <@@
         String.Format("{0} is dead, but {1} is alive! {0}", arg1, arg2)
      @@>

[<Test>]
let ``String.Empty works``() =
   check 
      <@@
         let s = String.Empty
         s
      @@>

[<Test>]
let ``sprintf works``() =
   check 
      <@@
         sprintf "%.2f %g" 0.5468989 5.  |> string
      @@>

[<Test>]
let ``String.Substring works``() =
   check 
      <@@ 
         "abcdefg".Substring(2)
      @@>

[<Test>]
let ``String.Substring works with length``() =
   check 
      <@@ 
         "abcdefg".Substring(2, 2)
      @@>

[<Test>]
let ``String.ToUpper and String.ToLower work``() =
   check 
      <@@ 
         "AbC".ToUpper() + "aBc".ToLower()
      @@>

[<Test>]
let ``String.Length works``() =
   check
      <@@ 
         "AbC".Length |> float
      @@>

[<Test>]
let ``String item works``() =
   checkAreEqual
      "b"
      <@@ 
         "AbC".[1]
      @@>

[<Test>]
let ``String.ToCharArray works``() =
   check 
      <@@
         let a = "abcd".ToCharArray()
         a.[0].ToString() + a.[1].ToString() + a.[2].ToString() + a.[3].ToString()
      @@>

[<Test>]    
let ``String.ToCharArray returns array``() =
   check 
      <@@
         "abcd".ToCharArray() |> Array.map (fun _ -> 1) |> Array.sum |> float
      @@>

[<Test>]
let ``String.Join works``() =
   check 
      <@@ 
         System.String.Join(",", [| "a"; "b"; "c" |])
      @@>


// String - F# module functions
 
[<TestCase("!!!"); TestCase("a!a"); TestCase("aaa")>]
let ``String.forall and exists work``(str) =
   check 
      <@@ 
         str |> String.forall (fun c -> charToInt c = charToInt '!') 
      @@> 

[<Test>]
let ``String.init works``() =
   check 
      <@@ 
         String.init 3 (fun i -> "a")
      @@> 

[<Test>]
let ``String.collect works``() =
   check 
      <@@ 
         "abc" |> String.collect (fun c -> "bcd")
      @@> 

[<Test>]
let ``String.iter works``() =
   check 
      <@@ 
         let res = ref ""
         "Hello world!" |> String.iter (fun c -> res := !res + c.ToString())
         !res
      @@> 

[<Test>]
let ``String.iteri works``() =
   check 
      <@@ 
         let res = ref ""
         "Hello world!" |> String.iteri (fun c i -> res := !res + i.ToString() + c.ToString())
         !res
      @@> 

[<Test>]
let ``String.length (function) works``() =
   check
      <@@ 
         "AbC" |> String.length |> float
      @@>

[<Test>]
let ``String.map works``() =
   check 
      <@@          
         "Hello world!" |> String.map (fun c -> if charToInt c = charToInt 'H' then '_' else c)
      @@> 

[<Test>]
let ``String.mapi works``() =
   check 
      <@@ 
         "Hello world!" |> String.mapi (fun i c -> if i = 1 || charToInt c = charToInt 'H' then '_' else c)
      @@> 

[<Test>]
let ``String.replicate works``() =
   check 
      <@@ 
         String.replicate 10 "hi there"
      @@> 
