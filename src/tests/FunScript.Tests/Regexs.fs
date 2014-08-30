[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Regexs

open NUnit.Framework
open System.Text.RegularExpressions


[<TestCase("Chapter \d+(\.\d)*"); TestCase("chapter \d+(\.\d)*")>]
let ``Regex.IsMatch works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         Regex.IsMatch(str, pattern)
      @@>

[<TestCase("chapter \d+(\.\d)*")>]
let ``Regex.IsMatch with IgnoreCase works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         Regex.IsMatch(str, pattern, RegexOptions.IgnoreCase)
      @@>

[<TestCase("^ab");TestCase("^cd")>]
let ``Regex.IsMatch with Multiline works``(pattern) =
   check 
      <@@ 
         let str = "ab\ncd"
         Regex.IsMatch(str, pattern, RegexOptions.Multiline)
      @@>

[<TestCase("Chapter \d+(\.\d)*"); TestCase("chapter \d+(\.\d)*")>]
let ``Regex.Match works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         let m = Regex.Match(str, pattern)
         m.Success
      @@>


[<TestCase("Chapter \d+(\.\d)*")>]
let ``Match.Groups indexer getter works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         let m = Regex.Match(str, pattern)
         let g = m.Groups.[1]
         g.Value
      @@>

[<TestCase("Chapter \d+(\.\d)*")>]
let ``Match.Groups iteration works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         let m = Regex.Match(str, pattern)
         let count = ref 0.
         for g in m.Groups do count := !count + 1.
         !count
      @@>

[<TestCase("Chapter \d+(\.\d)*")>]
let ``Match.Groups.Count works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         let m = Regex.Match(str, pattern)
         m.Groups.Count |> float
      @@>

[<TestCase("Chapter \d+(\.\d)*")>]
let ``Match.Index works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         let m = Regex.Match(str, pattern)
         m.Index |> float
      @@>

[<TestCase("Chapter \d+(\.\d)*")>]
let ``Match.Length works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         let m = Regex.Match(str, pattern)
         m.Length |> float
      @@>

[<TestCase("Chapter \d+(\.\d)*")>]
let ``Match.Value works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         let m = Regex.Match(str, pattern)
         m.Value
      @@>

[<TestCase("[A-E]")>]
let ``Regex.Matches indexer getter works``(pattern) =
   check 
      <@@ 
         let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         let ms = Regex.Matches(str, pattern, RegexOptions.IgnoreCase)
         ms.[8].Index |> float
      @@>

[<TestCase("[A-E]")>]
let ``Regex.Matches iteration works``(pattern) =
   check 
      <@@ 
         let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         let ms = Regex.Matches(str, pattern, RegexOptions.IgnoreCase)
         let count = ref 0.
         for m in ms do count := !count + 1.
         !count
      @@>

[<TestCase("[A-E]"); TestCase("XXX")>]
let ``MatchCollection.Count works``(pattern) =
   check 
      <@@ 
         let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         let ms = Regex.Matches(str, pattern, RegexOptions.IgnoreCase)
         ms.Count |> float
      @@>

[<TestCase("[;|,]")>]
let ``Regex.Split works``(pattern) =
   check 
      <@@ 
         let str = "plum;pear,orange"
         Regex.Split(str, pattern)
      @@>

[<TestCase("\\s+")>]
let ``Regex.Replace works``(pattern) =
   check 
      <@@ 
         let str = "This is   text with   far  too   much   \n whitespace"
         Regex.Replace(str, pattern, " ")
      @@>
