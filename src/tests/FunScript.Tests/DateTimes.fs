[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.DateTimes

open NUnit.Framework
open System

[<Test>]
let ``DateTime (year, month, day) constructor works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9)
         float d.Day
      @@>

[<Test>]
let ``DateTime (year, month, day, hour, minute, second) constructor works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30)
         float d.Second
      @@>

[<Test>]
let ``DateTime (year, month, day, hour, minute, second, millisecond) constructor works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
         float d.Millisecond
      @@>

[<Test>]
let ``DateTime.Now works``() =
   check 
      <@@ 
         let d = DateTime.Now
         float d.Hour
      @@>

// TODO: Make more test cases
[<Test>]
let ``DateTime.Parse works``() =
   check 
      <@@ 
         let d = DateTime.Parse("9/10/2014 1:50:34 PM")
         float (d.Year + d.Month + d.Day + d.Hour + d.Minute)
      @@>

[<Test>]
let ``DateTime.Day works``() =
   check 
      <@@ 
         let d = DateTime.Now
         float d.Day
      @@>

[<Test>]
let ``DateTime.DayOfWeek works``() =
   check 
      <@@ 
         let d = DateTime.Now
         float d.DayOfWeek
      @@>

[<Test>]
let ``DateTime.Hour works``() =
   check 
      <@@ 
         let d = DateTime.Now
         float d.Hour
      @@>

[<Test>]
let ``DateTime.Millisecond works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
         float d.Millisecond
      @@>

[<Test>]
let ``DateTime.Minute works``() =
   check 
      <@@ 
         let d = DateTime.Now
         float d.Minute
      @@>

[<Test>]
let ``DateTime.Month works``() =
   check 
      <@@ 
         let d = DateTime.Now
         float d.Month
      @@>

[<Test>]
let ``DateTime.Second works``() =
   check 
      <@@ 
         let d = DateTime.Now
         float d.Second
      @@>

[<Test>]
let ``DateTime.Year works``() =
   check 
      <@@ 
         let d = DateTime.Now
         float d.Year
      @@>

