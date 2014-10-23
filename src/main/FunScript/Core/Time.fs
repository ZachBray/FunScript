module FunScript.Core.Time

open FunScript

module Literals =
    let [<Literal>] millisecondsPerDay    = 86400000.
    let [<Literal>] millisecondsPerHour   = 3600000.
    let [<Literal>] millisecondsPerMinute = 60000.
    let [<Literal>] millisecondsPerSecond = 1000.
    let [<Literal>] millisecondsJSOffset  = 6.2135604e+13
    let [<Literal>] millisecondsMAX       = 8640000000000000.

// NOTE: For performance, TimeSpan is just a wrapper around the milliseconds
[<JS>]
type TimeSpan =
    member ts.TotalMilliseconds with get(): float        = unbox ts
    static member FromMilliseconds(ms: double): TimeSpan = unbox ms

    static member TicksPerDay         = 864000000000L
    static member TicksPerHour        = 36000000000L
    static member TicksPerMinute      = 600000000L
    static member TicksPerSecond      = 10000000L
    static member TicksPerMillisecond = 10000L
    static member Zero                = TimeSpan.FromMilliseconds(0.)
        
    static member FromDHMSM(d: int, h: int, m: int, s: int, ms: int) = 
        TimeSpan.FromMilliseconds <|
            (unbox d) * Literals.millisecondsPerDay    + (unbox h) * Literals.millisecondsPerHour   +
            (unbox m) * Literals.millisecondsPerMinute + (unbox s) * Literals.millisecondsPerSecond + (unbox ms)
    static member FromDHMS(d: int, h: int, m: int, s: int) = TimeSpan.FromDHMSM(d, h, m, s, 0)
    static member FromHMS(h: int, m: int, s: int)          = TimeSpan.FromDHMSM(0, h, m, s, 0)

    static member FromDays   (d: double)    = TimeSpan.FromMilliseconds(d * Literals.millisecondsPerDay)
    static member FromHours  (h: double)    = TimeSpan.FromMilliseconds(h * Literals.millisecondsPerHour)
    static member FromMinutes(m: double)    = TimeSpan.FromMilliseconds(m * Literals.millisecondsPerMinute)
    static member FromSeconds(s: double)    = TimeSpan.FromMilliseconds(s * Literals.millisecondsPerSecond)
    static member FromTicks  (ticks: int64) = TimeSpan.FromMilliseconds(unbox(ticks / TimeSpan.TicksPerMillisecond))

    member ts.Days         =  ts.TotalMilliseconds / Literals.millisecondsPerDay |> floor
    member ts.Hours        = (ts.TotalMilliseconds % Literals.millisecondsPerDay)    / Literals.millisecondsPerHour   |> floor
    member ts.Minutes      = (ts.TotalMilliseconds % Literals.millisecondsPerHour)   / Literals.millisecondsPerMinute |> floor
    member ts.Seconds      = (ts.TotalMilliseconds % Literals.millisecondsPerMinute) / Literals.millisecondsPerSecond |> floor
    member ts.Milliseconds = ts.TotalMilliseconds % Literals.millisecondsPerSecond |> floor

    member ts.Ticks: int64 = (unbox ts.TotalMilliseconds) * TimeSpan.TicksPerMillisecond

    member ts.TotalDays    = ts.TotalMilliseconds / Literals.millisecondsPerDay
    member ts.TotalHours   = ts.TotalMilliseconds / Literals.millisecondsPerHour
    member ts.TotalMinutes = ts.TotalMilliseconds / Literals.millisecondsPerMinute
    member ts.TotalSeconds = ts.TotalMilliseconds / Literals.millisecondsPerSecond

    member ts.Duration() = TimeSpan.FromMilliseconds(abs ts.TotalMilliseconds)
    member ts.Negate  () = TimeSpan.FromMilliseconds((~-) ts.TotalMilliseconds)

    member ts.Add     (that: TimeSpan)  = TimeSpan.FromMilliseconds(ts.TotalMilliseconds + that.TotalMilliseconds)
    member ts.Subtract(that: TimeSpan)  = TimeSpan.FromMilliseconds(ts.TotalMilliseconds - that.TotalMilliseconds)

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates")>]
    static member (+) (a: TimeSpan, b: TimeSpan) = a.Add(b)
    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates")>]
    static member (-) (a: TimeSpan, b: TimeSpan) = a.Subtract(b)

    override ts.GetHashCode() = 0
    override ts.Equals(that) = false
    static member Compare(a: TimeSpan, b: TimeSpan) = compare a.TotalMilliseconds b.TotalMilliseconds
    member ts.CompareTo  (that: TimeSpan)           = TimeSpan.Compare(ts, that)
    interface System.IComparable with 
        member ts.CompareTo(that: obj)              = TimeSpan.Compare(ts, unbox that)

// NOTE: For performance, DateTime is just a wrapper around the JS Date object (with an additional 'kind' property)
[<JS>]
type DateTime =
    // HELPER FUNCTIONS --------------------------------------------------------------------------
    [<JSEmit("""var date = {0} == null ? new Date() : new Date({0});
    if (isNaN(date)) { throw "The string was not recognized as a valid DateTime." }
    date.kind = {1};
    return date""")>]
    static member private createUnsafe(value: obj, kind: System.DateTimeKind): DateTime = failwith "never"

    [<JSEmit("""var date = {7} === {8} ? new Date(Date.UTC({0}, {1} - 1, {2}, {3}, {4}, {5}, {6})) : new Date({0}, {1} - 1, {2}, {3}, {4}, {5}, {6});
    if (isNaN(date)) { throw "The parameters describe an un-representable DateTime." }
    date.kind = {7};
    return date""")>]
    static member private createUnsafeYMDHMSM(year: int, month: int, day: int, h: int, min: int, s: int, ms: int, kind: System.DateTimeKind, utcKind: System.DateTimeKind): DateTime = failwith "never"

    [<JSEmit("if ({0}.kind == {1}) { return {0} } else { var newDate = new Date({0}.getTime()); newDate.kind = {1}; return newDate }")>]
    static member private changeKind(d: DateTime, kind: System.DateTimeKind): DateTime = failwith "never"

    [<JSEmit("return {0}.kind == {2} ? {0}['getUTC'+{1}]() : {0}['get'+{1}]()")>]
    static member private getValueUnsafe(d: DateTime, key: string, utcKind: System.DateTimeKind): int = failwith "never"

    [<JSEmitInline("{0}.getTime()")>]
    static member private getTime(d: DateTime): float = failwith "never"

    [<JSEmitInline("{0}.kind")>]
    static member private getKind(d: DateTime): System.DateTimeKind = failwith "never"

    [<JSEmitInline("(({0} % 4 == 0) && ({0} % 100 != 0)) || ({0} % 400 == 0)")>]
    static member isLeapYear(year: int): bool = failwith "never"

    [<JSEmitInline("{0}['to'+{1}+'String']()")>]
    static member private toFormattedStringUnsafe(d: DateTime, format: string): string = failwith "never"

    [<JSEmitInline("{0}.toLocaleTimeString().replace(/:\d\d(?!:)/, '')")>]
    static member private toShortTimeStringUnsafe(d: DateTime): string = failwith "never"

    static member private getDaysInMonths(year: int) =
        if DateTime.IsLeapYear(year)
        then [|31;29;31;30;31;30;31;31;30;31;30;31|]
        else [|31;28;31;30;31;30;31;31;30;31;30;31|]
    // -------------------------------------------------------------------------------------------

    static member FromYMDHMSMwithKind(year: int, month: int, day: int, h: int, min: int, s: int, ms: int, kind: System.DateTimeKind) =
        DateTime.createUnsafeYMDHMSM(year, month, day, h, min, s, ms, kind, System.DateTimeKind.Utc)

    static member FromYMDHMSM(year: int, month: int, day: int, h: int, min: int, s: int, ms: int) =
        DateTime.createUnsafeYMDHMSM(year, month, day, h, min, s, ms, System.DateTimeKind.Local, System.DateTimeKind.Utc)

    static member FromYMDHMSwithKind(year: int, month: int, day: int, h: int, min: int, s: int, kind: System.DateTimeKind) =
        DateTime.createUnsafeYMDHMSM(year, month, day, h, min, s, 0, kind, System.DateTimeKind.Utc)

    static member FromYMDHMS(year: int, month: int, day: int, h: int, min: int, s: int) =
        DateTime.createUnsafeYMDHMSM(year, month, day, h, min, s, 0, System.DateTimeKind.Local, System.DateTimeKind.Utc)

    static member FromYMD(year: int, month: int, day: int): DateTime =
        DateTime.createUnsafeYMDHMSM(year, month, day, 0, 0, 0, 0, System.DateTimeKind.Local, System.DateTimeKind.Utc)

    static member MinValue with get() = DateTime.createUnsafe(-Literals.millisecondsMAX, System.DateTimeKind.Utc)
    static member MaxValue with get() = DateTime.createUnsafe(Literals.millisecondsMAX, System.DateTimeKind.Utc)

    static member Now    with get() = DateTime.createUnsafe(null, System.DateTimeKind.Local)
    static member UtcNow with get() = DateTime.createUnsafe(null, System.DateTimeKind.Utc)
    static member Today  with get() = DateTime.Now.Date
    static member Parse (s: string) = DateTime.createUnsafe(s, System.DateTimeKind.Local)

    static member IsLeapYear(year: int): bool =
        DateTime.isLeapYear(year)

    static member DaysInMonth(year: int, month: int) =
        DateTime.getDaysInMonths(year).[month - 1]

    member dt.ToUniversalTime() = DateTime.changeKind(dt, System.DateTimeKind.Utc)
    member dt.ToLocalTime    () = DateTime.changeKind(dt, System.DateTimeKind.Local)

    member dt.TimeOfDay with get() = TimeSpan.FromHMS(dt.Hour, dt.Minute, dt.Second)
    member dt.Date      with get() = DateTime.createUnsafeYMDHMSM(dt.Year, dt.Month, dt.Day, 0, 0, 0, 0, dt.Kind, System.DateTimeKind.Utc)

    member dt.Day           with get() = DateTime.getValueUnsafe(dt, "Date",         System.DateTimeKind.Utc)
    member dt.Hour          with get() = DateTime.getValueUnsafe(dt, "Hours",        System.DateTimeKind.Utc)
    member dt.Millisecond   with get() = DateTime.getValueUnsafe(dt, "Milliseconds", System.DateTimeKind.Utc)
    member dt.Minute        with get() = DateTime.getValueUnsafe(dt, "Minutes",      System.DateTimeKind.Utc)
    member dt.Month         with get() = DateTime.getValueUnsafe(dt, "Month",        System.DateTimeKind.Utc) + 1
    member dt.Second        with get() = DateTime.getValueUnsafe(dt, "Seconds",      System.DateTimeKind.Utc)
    member dt.Year          with get() = DateTime.getValueUnsafe(dt, "FullYear",     System.DateTimeKind.Utc)
    member dt.Kind          with get() = DateTime.getKind(dt)
    member dt.Ticks         with get() = unbox<int64>((DateTime.getTime(dt) + Literals.millisecondsJSOffset) * (unbox TimeSpan.TicksPerMillisecond))
    member dt.DayOfWeek     with get() = unbox<System.DayOfWeek>(DateTime.getValueUnsafe(dt, "Day", System.DateTimeKind.Utc))
    member dt.DayOfYear     with get() = DateTime.getDaysInMonths(dt.Year)
                                         |> Seq.take (dt.Month - 1)
                                         |> Seq.fold (+) dt.Day

    member dt.Add(t: TimeSpan)          = DateTime.createUnsafe(DateTime.getTime(dt) + t.TotalMilliseconds, dt.Kind)
    member dt.AddDays(v: float)         = DateTime.createUnsafe(DateTime.getTime(dt) + v * Literals.millisecondsPerDay, dt.Kind)
    member dt.AddHours(v: float)        = DateTime.createUnsafe(DateTime.getTime(dt) + v * Literals.millisecondsPerHour, dt.Kind)
    member dt.AddMinutes(v: float)      = DateTime.createUnsafe(DateTime.getTime(dt) + v * Literals.millisecondsPerMinute, dt.Kind)
    member dt.AddSeconds(v: float)      = DateTime.createUnsafe(DateTime.getTime(dt) + v * Literals.millisecondsPerSecond, dt.Kind)
    member dt.AddMilliseconds(v: float) = DateTime.createUnsafe(DateTime.getTime(dt) + v, dt.Kind)
    member dt.AddTicks(v: int64)        = DateTime.createUnsafe(DateTime.getTime(dt) + float (v / TimeSpan.TicksPerMillisecond), dt.Kind)

    member dt.AddYears(offset: int) =
        let newMonth = dt.Month
        let newYear = dt.Year + (unbox offset)
        let daysInMonth = DateTime.DaysInMonth(newYear, newMonth)
        let newDay = min daysInMonth dt.Day
        DateTime.createUnsafeYMDHMSM(newYear, newMonth, newDay, dt.Hour, dt.Minute, dt.Second, dt.Millisecond, dt.Kind, System.DateTimeKind.Utc)

    member dt.AddMonths(offset: int) =
        let newMonth = dt.Month + offset
        let newMonth, yearOffset =
            if newMonth > 12 then
                let newMonth' = newMonth % 12
                let yearOffset = (float newMonth) / 12. |> floor
                newMonth', yearOffset
            elif newMonth < 1 then
                let newMonth' = 12 + (newMonth % 12)
                let yearOffset = (float newMonth) / 12. |> floor |> (+) (if newMonth' = 12 then -1. else 0.)
                newMonth', yearOffset
            else
                newMonth, 0.
        let newYear = dt.Year + (unbox yearOffset)
        let daysInMonth = DateTime.DaysInMonth(newYear, newMonth)
        let newDay = min daysInMonth dt.Day
        DateTime.createUnsafeYMDHMSM(newYear, newMonth, newDay, dt.Hour, dt.Minute, dt.Second, dt.Millisecond, dt.Kind, System.DateTimeKind.Utc)

    static member SubtractTimeSpan(dt: DateTime, t: TimeSpan)    = DateTime.createUnsafe(DateTime.getTime(dt) - t.TotalMilliseconds, dt.Kind)
    static member SubtractDateTime(dt: DateTime, that: DateTime): TimeSpan = TimeSpan.FromMilliseconds((DateTime.getTime dt) - (DateTime.getTime that))

//    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates")>]
//    static member (+) (a: DateTime, b: TimeSpan) = a.Add(b)
//    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates")>]
//    static member (-) (a: DateTime, b: TimeSpan) = DateTime.SubtractTimeSpan(a, b)
//    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates")>]
//    static member (-) (a: DateTime, b: DateTime) = DateTime.SubtractDateTime(a, b)

    member dt.ToLongDateString()    = DateTime.toFormattedStringUnsafe(dt, "Date")
    member dt.ToShortDateString()   = DateTime.toFormattedStringUnsafe(dt, "LocaleDate")
    member dt.ToLongTimeString()    = DateTime.toFormattedStringUnsafe(dt, "LocaleTime")
    member dt.ToShortTimeString()   = DateTime.toShortTimeStringUnsafe(dt)

    override dt.GetHashCode() = unbox<int>(DateTime.getTime(dt))
    override dt.Equals(that)  = DateTime.getTime(dt) = DateTime.getTime(unbox that)

    static member Compare(a: DateTime, b: DateTime) = compare (DateTime.getTime a) (DateTime.getTime b)
    member dt.CompareTo  (that: DateTime)           = DateTime.Compare(dt, that)
    interface System.IComparable with 
        member dt.CompareTo(that: obj)              = DateTime.Compare(dt, unbox that)
