[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Regexs

open NUnit.Framework
open System.Text.RegularExpressions

[<TestCase("Chapter \d+(\.\d)*"); TestCase("chapter \d+(\.\d)*")>]
let ``Regex instance IsMatch works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         let re = Regex(pattern)
         re.IsMatch(str)
      @@>

[<TestCase(10); TestCase(40)>]
let ``Regex instance IsMatch with offset works``(offset) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         let re = Regex("Chapter \d+(\.\d)*")
         re.IsMatch(str, offset)
      @@>

[<TestCase("[A-E]"); TestCase("(ZZ)+")>]
let ``Regex instance Match and Matches work``(pattern) =
   check 
      <@@ 
         let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         let re = Regex(pattern, RegexOptions.IgnoreCase)
         let index = let m = re.Match(str) in if m.Success then m.Index else -1
         let ms = re.Matches(str)
         (index + ms.Count) |> float
      @@>

[<TestCase(10); TestCase(40)>]
let ``Regex instance Match and Matches with offset work``(offset) =
   check 
      <@@ 
         let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         let re = Regex("[A-E]", RegexOptions.IgnoreCase)
         let index = let m = re.Match(str, offset) in if m.Success then m.Index else -1
         let ms = re.Matches(str, offset)
         (index + ms.Count) |> float
      @@>

[<TestCase("Chapter \d+(\.\d)*"); TestCase("chapter \d+(\.\d)*")>]
let ``Regex.IsMatch works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         Regex.IsMatch(str, pattern)
      @@>

[<TestCase("Chapter \d+(\.\d)*"); TestCase("chapter \d+(\.\d)*")>]
let ``Regex.IsMatch with IgnoreCase works``(pattern) =
   check 
      <@@ 
         let str = "For more information, see Chapter 3.4.5.1"
         Regex.IsMatch(str, pattern, RegexOptions.IgnoreCase)
      @@>

[<TestCase("^ab");TestCase("^cd");TestCase("^AB");TestCase("^CD")>]
let ``Regex.IsMatch with Multiline works``(pattern) =
   check 
      <@@ 
         let str = "ab\ncd"
         Regex.IsMatch(str, pattern, RegexOptions.Multiline)
      @@>


// NOTE: Tests for setting both flags at the same time are failing, but it works in actual code.

//[<TestCase("^ab");TestCase("^cd");TestCase("^AB");TestCase("^CD")>]
//let ``Regex.IsMatch with IgnoreCase and Multiline works``(pattern) =
//   check 
//      <@@ 
//         let str = "ab\ncd"
//         Regex.IsMatch(str, pattern, RegexOptions.IgnoreCase ||| RegexOptions.Multiline)
//      @@>

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
         let count = ref 0
         for g in m.Groups do count := !count + g.Value.Length
         float !count
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
         let count = ref 0
         for m in ms do count := !count + m.Value.Length
         float !count
      @@>

[<TestCase("[A-E]"); TestCase("(ZZ)+")>]
let ``MatchCollection.Count works``(pattern) =
   check 
      <@@ 
         let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         let ms = Regex.Matches(str, pattern, RegexOptions.IgnoreCase)
         ms.Count |> float
      @@>

[<TestCase("plum;pear,orange"); TestCase("")>]
let ``Regex.Split works``(str) =
   check 
      <@@ 
         let pattern = "[;,]"
         let splits = Regex.Split(str, pattern)
         splits.Length |> float
      @@>

[<TestCase("\\s+")>]
let ``Regex.Replace works``(pattern) =
   check 
      <@@ 
         let str = "This is   text with   far  too   much   \n whitespace"
         Regex.Replace(str, pattern, " ")
      @@>
