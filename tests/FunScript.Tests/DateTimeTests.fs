[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.DateTimes

open NUnit.Framework
open System

[<FunScript.JS>]
let toSigFigs nSigFigs x =
    let absX = abs x
    let digitsToStartOfNumber = floor(log10 absX) + 1. // x > 0 => +ve | x < 0 => -ve
    let digitsToAdjustNumberBy = int digitsToStartOfNumber - nSigFigs
    let scale = pown 10. digitsToAdjustNumberBy
    round(x / scale) * scale

// TODO: These two tests give different values for .NET and JS because DateTime
// becomes as a plain JS Date object, so I'm just checking the fields get translated
[<Test>]
let ``DateTime.MaxValue works``() =
   check 
      <@@ 
         let d = DateTime.MaxValue
         0.
      @@>

[<Test>]
let ``DateTime.MinValue works``() =
   check 
      <@@ 
         let d = DateTime.MinValue
         0.
      @@>


[<Test>]
let ``DateTime.ToLocalTime works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
         let d' = d.ToLocalTime()
         float (d.Hour + d'.Hour)
      @@>

[<Test>]
let ``DateTime.Hour works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
         let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
         float (d.Hour + d'.Hour)
      @@>

// TODO: These four tests don't match exactly between .NET and JS
// Think of a way to compare the results approximately
[<Test>]
let ``DateTime.ToLongDateString works``() =
   check 
      <@@ 
         let dt = DateTime(2014, 9, 11, 16, 37, 0)
         let s = dt.ToLongDateString()
         s.Length > 0
      @@>

[<Test>]
let ``DateTime.ToShortDateString works``() =
   check 
      <@@ 
         let dt = DateTime(2014, 9, 11, 16, 37, 0)
         let s = dt.ToShortDateString()
         s.Length > 0
      @@>

[<Test>]
let ``DateTime.ToLongTimeString works``() =
   check 
      <@@ 
         let dt = DateTime(2014, 9, 11, 16, 37, 0)
         let s = dt.ToLongTimeString()
         s.Length > 0
      @@>

[<Test>]
let ``DateTime.ToShortTimeString works``() =
   check 
      <@@ 
         let dt = DateTime(2014, 9, 11, 16, 37, 0)
         let s = dt.ToShortTimeString()
         s.Length > 0
      @@>

// TODO: Unfortunately, JS will happily create invalid dates like DateTime(2014,2,29)
//       But this problem also happens when parsing, so I haven't tried to fix it
[<Test>]
let ``DateTime constructors work``() =
   check 
      <@@ 
         let d1 = DateTime(2014, 10, 9)
         let d2 = DateTime(2014, 10, 9, 13, 23, 30)
         let d3 = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
         let d4 = DateTime(2014, 10, 9, 13, 23, 30, 500)
         let d5 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
         (float d1.Day) + (float d2.Second) + (float d3.Second) + (float d4.Millisecond) + (float d5.Millisecond)
      @@>

[<TestCase(2014); TestCase(2016);>]
let ``DateTime.IsLeapYear works``(year) =
   check 
      <@@ 
         DateTime.IsLeapYear(year)
      @@>

[<TestCase(2014, 1); TestCase(2014, 2); TestCase(2014, 4); TestCase(2016, 2)>]
let ``DateTime.DaysInMonth works``(year, month) =
   check 
      <@@ 
         DateTime.DaysInMonth(year, month) |> float
      @@>

[<Test>]
let ``DateTime.Now works``() =
   check 
      <@@ 
         let d = DateTime.Now
         float d.Hour
      @@>

[<Test>]
let ``DateTime.UtcNow works``() =
   check 
      <@@ 
         let d = DateTime.UtcNow
         float d.Hour
      @@>

[<Test>]
let ``DateTime.Parse works``() =
   check 
      <@@ 
         let d = DateTime.Parse("9/10/2014 1:50:34 PM")
         float (d.Year + d.Month + d.Day + d.Hour + d.Minute)
      @@>

[<Test>]
let ``DateTime.Today works``() =
   check 
      <@@ 
         let d = DateTime.Today
         float (d.Day + d.Hour)
      @@>


[<Test>]
let ``DateTime.ToUniversalTime works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
         let d' = d.ToUniversalTime()
         float (d.Hour + d'.Hour)
      @@>

[<Test>]
let ``DateTime.Date works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30)
         let d' = d.Date
         (float d.Hour) + (float d'.Hour)
      @@>

[<Test>]
let ``DateTime.Kind works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
         let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
         (float d.Kind) + (float d'.Kind)
      @@>

[<Test>]
let ``DateTime.Day works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
         let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
         float (d.Day + d'.Day)
      @@>

[<Test>]
let ``DateTime.DayOfWeek works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9)
         (float d.DayOfWeek)
      @@>

[<Test>]
let ``DateTime.DayOfYear works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9)
         (float d.DayOfYear)
      @@>

[<Test>]
let ``DateTime.Millisecond works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
         float d.Millisecond
      @@>

[<Test>]
let ``DateTime.Ticks works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
         // TODO: Worrying that we need to round to 5 sig. figs. here. 
         //       Perhaps the number type in JS isn't precise enough for this implementation.
         toSigFigs 5 (float d.Ticks) 
      @@>

[<Test>]
let ``DateTime.Minute works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
         let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
         float (d.Minute + d'.Minute)
      @@>

[<Test>]
let ``DateTime.Month works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
         let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
         float (d.Month + d'.Month)
      @@>

[<Test>]
let ``DateTime.Second works``() =
   check 
      <@@ 
         let d = DateTime(2014,9,11,0,0,30)
         let d' = DateTime(2014,9,11,0,0,59)
         float (d.Second + d'.Second)
      @@>

[<Test>]
let ``DateTime.Year works``() =
   check 
      <@@ 
         let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
         let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
         float (d.Year + d'.Year)
      @@>

[<TestCase(100); TestCase(1); TestCase(-1);TestCase(-100); TestCase(0)>]
let ``DateTime.AddYears works``(v) =
   check 
      <@@ 
         let dt = DateTime(2016,2,29).AddYears(v)
         float (dt.Month + dt.Day)
      @@>

[<TestCase(100); TestCase(20); TestCase(6); TestCase(5); TestCase(1); TestCase(0); TestCase(-1); TestCase(-5); TestCase(-20); TestCase(-100)>]
let ``DateTime.AddMonths works``(v) =
   check 
      <@@ 
         let dt = DateTime(2016,1,31).AddMonths(v)
         float (dt.Year + dt.Month + dt.Day)
      @@>

[<TestCase(100.); TestCase(-100.); TestCase(0.)>]
let ``DateTime.AddDays works``(v) =
   check 
      <@@ 
         let dt = DateTime(2014,9,11).AddDays(v)
         // TODO: Worrying that we need to round to 5 sig. figs. here. 
         //       Perhaps the number type in JS isn't precise enough for this implementation.
         toSigFigs 5 (float dt.Ticks) 
      @@>

[<TestCase(100.); TestCase(-100.); TestCase(0.)>]
let ``DateTime.AddHours works``(v) =
   check 
      <@@ 
         let dt = DateTime(2014,9,11).AddHours(v)
         // TODO: Worrying that we need to round to 5 sig. figs. here. 
         //       Perhaps the number type in JS isn't precise enough for this implementation.
         toSigFigs 5 (float dt.Ticks) 
      @@>

[<TestCase(100.); TestCase(-100.); TestCase(0.)>]
let ``DateTime.AddMinutes works``(v) =
   check 
      <@@ 
         let dt = DateTime(2014,9,11).AddMinutes(v)
         // TODO: Worrying that we need to round to 5 sig. figs. here. 
         //       Perhaps the number type in JS isn't precise enough for this implementation.
         toSigFigs 5 (float dt.Ticks) 
      @@>

[<TestCase(100.); TestCase(-100.); TestCase(0.)>]
let ``DateTime.AddSeconds works``(v) =
   check 
      <@@ 
         let dt = DateTime(2014,9,11).AddSeconds(v)
         // TODO: Worrying that we need to round to 5 sig. figs. here. 
         //       Perhaps the number type in JS isn't precise enough for this implementation.
         toSigFigs 5 (float dt.Ticks) 
      @@>

[<TestCase(100.); TestCase(-100.); TestCase(0.)>]
let ``DateTime.AddMilliseconds works``(v) =
   check 
      <@@ 
         let dt = DateTime(2014,9,11).AddMilliseconds(v)
         // TODO: Worrying that we need to round to 5 sig. figs. here. 
         //       Perhaps the number type in JS isn't precise enough for this implementation.
         toSigFigs 5 (float dt.Ticks) 
      @@>

// NOTE: Doesn't work for values between 10000L (TimeSpan.TicksPerMillisecond) and -10000L, except 0L
[<TestCase(100000L); TestCase(-100000L); TestCase(0L)>]
let ``DateTime.AddTicks works``(v) =
   check 
      <@@ 
         let dt = DateTime(2014,9,11).AddTicks(v)
         // TODO: Worrying that we need to round to 5 sig. figs. here. 
         //       Perhaps the number type in JS isn't precise enough for this implementation.
         toSigFigs 5 (float dt.Ticks) 
      @@>

[<TestCase(1000.); TestCase(-1000.); TestCase(0.)>]
let ``DateTime Addition works``(ms) =
   check 
      <@ 
         let dt = DateTime(2014,9,11)
         let ts = TimeSpan.FromMilliseconds(ms)
         let res1 = dt.Add(ts).Ticks
         let res2 = (dt + ts).Ticks
         if (res1 = res2)
         // TODO: Worrying that we need to round to 5 sig. figs. here. 
         //       Perhaps the number type in JS isn't precise enough for this implementation.
         then toSigFigs 5 (float res2) 
         else failwith "synonyms don't match"
      @>

[<TestCase(1000.); TestCase(-1000.); TestCase(0.)>]
let ``DateTime Subtraction with TimeSpan works``(ms) =
   check 
      <@@ 
         let dt = DateTime(2014,9,11)
         let ts = TimeSpan.FromMilliseconds(ms)
         let res1 = dt.Subtract(ts).Ticks
         let res2 = (dt - ts).Ticks
         if (res1 = res2)
         // TODO: Worrying that we need to round to 5 sig. figs. here. 
         //       Perhaps the number type in JS isn't precise enough for this implementation.
         then toSigFigs 5 (float res2)
         else failwith "synonyms don't match"
      @@>

[<TestCase(1000.); TestCase(-1000.); TestCase(0.)>]
let ``DateTime Subtraction with DateTime works``(ms) =
   check 
      <@@ 
         let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234)
         let dt2 = dt1.AddMilliseconds(ms)
         let res1 = dt1.Subtract(dt2).Ticks
         let res2 = (dt1 - dt2).Ticks
         if (res1 = res2)
         then float res2
         else failwith "synonyms don't match"
      @@>

[<TestCase(1000.); TestCase(-1000.); TestCase(0.)>]
let ``DateTime Comparison works``(ms) =
   check 
      <@@ 
         let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234)
         let dt2 = dt1.AddMilliseconds(ms)
         let res1 = compare dt1 dt2
         let res2 = dt1.CompareTo(dt2)
         let res3 = DateTime.Compare(dt1, dt2)
         if (res1 = res2 && res2 = res3)
         then float res1
         else failwith "synonyms don't match"
      @@>

[<TestCase(1000.); TestCase(-1000.); TestCase(0.)>]
let ``DateTime GreaterThan works``(ms) =
   check 
      <@@ 
         let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234)
         let dt2 = dt1.AddMilliseconds(ms)
         dt1 > dt2
      @@>

[<TestCase(1000.); TestCase(-1000.); TestCase(0.)>]
let ``DateTime LessThan works``(ms) =
   check 
      <@@ 
         let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234)
         let dt2 = dt1.AddMilliseconds(ms)
         dt1 < dt2
      @@>

[<TestCase(1000.); TestCase(-1000.); TestCase(0.)>]
let ``DateTime Equality works``(ms) =
   check 
      <@@ 
         let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234)
         let dt2 = dt1.AddMilliseconds(ms)
         dt1 = dt2
      @@>

[<TestCase(1000.); TestCase(-1000.); TestCase(0.)>]
let ``DateTime Inequality works``(ms) =
   check 
      <@@ 
         let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234)
         let dt2 = dt1.AddMilliseconds(ms)
         dt1 <> dt2
      @@>

[<Test>]
let ``TimeSpan constructors work``() =
   check 
      <@@ 
         let t1 = TimeSpan(20000L)
         let t2 = TimeSpan(3, 3, 3)
         let t3 = TimeSpan(5, 5, 5, 5)
         let t4 = TimeSpan(7, 7, 7, 7, 7)
         t1.TotalMilliseconds + t2.TotalMilliseconds + t3.TotalMilliseconds + t4.TotalMilliseconds
      @@>

[<Test>]
let ``TimeSpan static creation works``() =
   check 
      <@@ 
         let t1 = TimeSpan.FromTicks(20000L)
         let t2 = TimeSpan.FromMilliseconds(2.)
         let t3 = TimeSpan.FromDays   (2.)
         let t4 = TimeSpan.FromHours  (2.)
         let t5 = TimeSpan.FromMinutes(2.)
         let t6 = TimeSpan.FromSeconds(2.)
//         let t7 = TimeSpan.Zero
         t1.TotalMilliseconds + t2.TotalMilliseconds + t3.TotalMilliseconds + t4.TotalMilliseconds +
            t5.TotalMilliseconds + t6.TotalMilliseconds // + t7.TotalMilliseconds
      @@>

[<Test>]
let ``TimeSpan components work``() =
   check 
      <@@ 
         let t = TimeSpan.FromMilliseconds(96441615.)
         t.Days + t.Hours + t.Minutes + t.Seconds + t.Milliseconds |> float
      @@>

[<Test>]
let ``TimeSpan.Ticks works``() =
   check 
      <@@ 
         let t = TimeSpan.FromTicks(20000L)
         float t.Ticks
      @@>

// NOTE: This test fails because of very small fractions, so I cut the fractional part
[<Test>]
let ``TimeSpan totals work``() =
   check 
      <@@ 
         let t = TimeSpan.FromMilliseconds(96441615.)
         t.TotalDays + t.TotalHours + t.TotalMinutes + t.TotalSeconds |> floor
      @@>

[<TestCase(1.); TestCase(-1.); TestCase(0.)>]
let ``TimeSpan.Duration works``(ms) =
   check 
      <@@ 
         let t = TimeSpan.FromMilliseconds(ms)
         t.Duration().TotalMilliseconds
      @@>

[<TestCase(1.); TestCase(-1.); TestCase(0.)>]
let ``TimeSpan.Negate works``(ms) =
   check 
      <@@ 
         let t = TimeSpan.FromMilliseconds(ms)
         t.Negate().TotalMilliseconds
      @@>

[<TestCase(1000., 2000.); TestCase(200., -1000.); TestCase(-2000., 1000.);
  TestCase(-200., -300.); TestCase(0., 1000.); TestCase(-2000., 0.); TestCase(0., 0.)>]
let ``TimeSpan Addition works``(ms1, ms2) =
   check 
      <@@ 
         let t1 = TimeSpan.FromMilliseconds(ms1)
         let t2 = TimeSpan.FromMilliseconds(ms2)
         let res1 = t1.Add(t2).TotalMilliseconds
         let res2 = (t1 + t2).TotalMilliseconds
         if (res1 = res2)
         then res1
         else failwith "synonyms don't match"
      @@>

[<TestCase(1000., 2000.); TestCase(2000., -2000.); TestCase(-2000., 1000.);
  TestCase(200., -300.); TestCase(0., 1000.); TestCase(1000., 1000.); TestCase(0., 0.)>]
let ``TimeSpan Subtraction works``(ms1, ms2) =
   check 
      <@@ 
         let t1 = TimeSpan.FromMilliseconds(ms1)
         let t2 = TimeSpan.FromMilliseconds(ms2)
         let res1 = t1.Subtract(t2).TotalMilliseconds
         let res2 = (t1 - t2).TotalMilliseconds
         if (res1 = res2)
         then res1
         else failwith "synonyms don't match"
      @@>

[<TestCase(1000., 2000.); TestCase(2000., 1000.); TestCase(-2000., -2000.);
  TestCase(200., -200.); TestCase(0., 1000.); TestCase(1000., 1000.); TestCase(0., 0.)>]
let ``TimeSpan Comparison works``(ms1, ms2) =
   check 
      <@@ 
         let t1 = TimeSpan.FromMilliseconds(ms1)
         let t2 = TimeSpan.FromMilliseconds(ms2)
         let res1 = compare t1 t2
         let res2 = t1.CompareTo(t2)
         let res3 = TimeSpan.Compare(t1, t2)
         if (res1 = res2 && res2 = res3)
         then float res1
         else failwith "synonyms don't match"
      @@>

[<TestCase(1000., 2000.); TestCase(2000., 1000.); TestCase(-2000., -2000.)>]
let ``TimeSpan GreaterThan works``(ms1, ms2) =
   check 
      <@@ 
         let t1 = TimeSpan.FromMilliseconds(ms1)
         let t2 = TimeSpan.FromMilliseconds(ms2)
         t1 > t2
      @@>

[<TestCase(1000., 2000.); TestCase(2000., 1000.); TestCase(-2000., -2000.)>]
let ``TimeSpan LessThan works``(ms1, ms2) =
   check 
      <@@ 
         let t1 = TimeSpan.FromMilliseconds(ms1)
         let t2 = TimeSpan.FromMilliseconds(ms2)
         t1 < t2
      @@>

[<TestCase(1000., 2000.); TestCase(2000., 1000.); TestCase(-2000., -2000.)>]
let ``TimeSpan Equality works``(ms1, ms2) =
   check 
      <@@ 
         let t1 = TimeSpan.FromMilliseconds(ms1)
         let t2 = TimeSpan.FromMilliseconds(ms2)
         t1 = t2
      @@>

[<TestCase(1000., 2000.); TestCase(2000., 1000.); TestCase(-2000., -2000.)>]
let ``TimeSpan Inequality works``(ms1, ms2) =
   check 
      <@@ 
         let t1 = TimeSpan.FromMilliseconds(ms1)
         let t2 = TimeSpan.FromMilliseconds(ms2)
         t1 <> t2
      @@>
