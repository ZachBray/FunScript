[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Regexs

open NUnit.Framework
open System.Text.RegularExpressions

// NOTE: In Jint we cannot use the ||| operator with RegexOptions
// but it works fine in the browser
[<Test>]
let ``Regex.Options works``() =
   check 
      <@@
         let option1 = int RegexOptions.IgnoreCase
         let option2 = int RegexOptions.ECMAScript
         let options = option1 ||| option2
         let r = Regex("[a-z]", unbox options)
         r.Options |> float
      @@>

[<TestCase("^ab");TestCase("^cd");TestCase("^AB");TestCase("^CD")>]
let ``Regex.IsMatch with IgnoreCase and Multiline works``(pattern) =
   check 
      <@@ 
         let str = "ab\ncd"
         let option1 = int RegexOptions.IgnoreCase
         let option2 = int RegexOptions.Multiline
         let options = option1 ||| option2
         Regex.IsMatch(str, pattern, unbox options)
      @@>

[<TestCase("[(.*?)]"); TestCase(@"C:\Temp")>]
let ``Regex.Escape works``(str) =
   check 
      <@@ 
         Regex.Escape(str)
      @@>

[<TestCase("\[\(\.\*\?\)]"); TestCase(@"C:\\Temp")>]
let ``Regex.Unescape works``(str) =
   check 
      <@@ 
         Regex.Unescape(str)
      @@>

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
         let m = Regex.Match(str, "Chapter \d+(\.\d)*")
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

[<TestCase("[A-E]")>]
let ``Regex.Matches iteration with casting works``(pattern) =
   check 
      <@@ 
         let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         let ms = Regex.Matches(str, pattern, RegexOptions.IgnoreCase)
         let count =
            ms |> Seq.cast<Match> |> Seq.fold(fun acc m -> acc + m.Value.Length) 0
         float count
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

[<TestCase(1); TestCase(3)>]
let ``Regex.Split with limit works``(count) =
   check 
      <@@ 
         let s = "blah blah blah, blah blah blah"
         let r = Regex(" ")
         let splits = r.Split(s, count)
         splits.Length |> float
      @@>

[<TestCase(0); TestCase(20)>]
let ``Regex.Split with limit and offset works``(limit) =
   check 
      <@@ 
         let s = "blah blah blah, blah blah blah"
         let r = Regex(" ")
         let splits = r.Split(s, 10, limit)
         splits.Length |> float
      @@>

[<TestCase("\\s+"); TestCase("")>]
let ``Regex.Replace works``(pattern) =
   check 
      <@@ 
         let str = "This is   text with   far  too   much   \n whitespace"
         Regex.Replace(str, pattern, " ")
      @@>

[<TestCase("([A-Za-z]+) ([A-Za-z\-]+)"); TestCase("(fon)(so)")>]
let ``Regex.Replace with macros works``(pattern) =
   check 
      <@@ 
         let str = "Names: Zach Bray, Alfonso Garcia-Caro"
         Regex.Replace(str, pattern, "$2 $1")
      @@>

[<TestCase(1); TestCase(3)>]
let ``Regex.Replace with limit works``(count) =
   check 
      <@@ 
         let str = "This   is   text with   far  too   much   \n whitespace"
         let r = Regex("\\s+")
         r.Replace(str, " ", count=count)
      @@>

[<TestCase(0); TestCase(20)>]
let ``Regex.Replace with limit and offset works``(startat) =
   check 
      <@@ 
         let str = "This   is   text with   far  too   much   \n whitespace"
         let r = Regex("\\s+")
         r.Replace(str, " ", count=20, startat=startat)
      @@>

[<Test>]
let ``Regex.Replace with limit, offset and macros works``() =
   check 
      <@@ 
         let str = "Names: Zach Bray, Alfonso Garcia-Caro"
         let re = Regex("([A-Za-z]+) ([A-Za-z\-]+)")
         let res1 = re.Replace(str, "$2 $1", 1)
         let res2 = re.Replace(str, "$2 $1", 10, 20)
         res1 + " - " + res2
      @@>

[<TestCase("([A-Za-z]+) ([A-Za-z\-]+)"); TestCase("(fon)(so)")>]
let ``Regex.Replace with evaluator works``(pattern) =
   check 
      <@@ 
         let str = "Names: Zach Bray, Alfonso Garcia-Caro"
         Regex.Replace(str, pattern, fun (m: Match) -> m.Groups.[2].Value + " " + m.Groups.[1].Value)
      @@>

[<TestCase(1); TestCase(3)>]
let ``Regex.Replace with evaluator and limit works``(count) =
   check 
      <@@ 
         let str = "abcabcabcabcabcabcabcabc"
         let r = Regex("c")
         r.Replace(str, (fun (m: Match) -> string m.Index), count=count)
      @@>

[<TestCase(0); TestCase(10)>]
let ``Regex.Replace with evaluator, limit and offset works``(startat) =
   check 
      <@@ 
         let str = "abcCcabCCabcccabcabcabCCCcabcabc"
         let r = Regex("c+", RegexOptions.IgnoreCase)
         r.Replace(str, (fun (m: Match) -> string m.Length), count=3, startat=startat)
      @@>