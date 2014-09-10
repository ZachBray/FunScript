[<ReflectedDefinition>]
module FunScript.Core.DateTime

open System
open FunScript

// TODO: Add UTC (and DateTimeKind?), TimeSpan type, operations and comparison

[<JSEmitInline("(new Date({0}, {1} - 1, {2}, 0, 0, 0, 0))")>]
let CreateYMD(year: int, month: int, day: int) = DateTime()

[<JSEmitInline("(new Date({0}, {1} - 1, {2}, {3}, {4}, {5}, 0))")>]
let CreateYMDHMS(year: int, month: int, day: int, hour: int, minute: int, second: int) = DateTime()

[<JSEmitInline("(new Date({0}, {1} - 1, {2}, {3}, {4}, {5}, {6}))")>]
let CreateYMDHMSM(year: int, month: int, day: int, hour: int, minute: int, second: int, millisecond: int) = DateTime()

[<JSEmitInline("(new Date())")>]
let Now(): DateTime = failwith "never"

[<JSEmitInline("(new Date({0}))")>]
let Parse(s: string): DateTime = failwith "never"

[<JSEmitInline("{0}.getDate()")>]
let Day(d: DateTime): int = failwith "never"

[<JSEmitInline("{0}.getDay()")>]
let DayOfWeek(d: DateTime): System.DayOfWeek = failwith "never"

[<JSEmitInline("{0}.getHours()")>]
let Hour(d: DateTime): int = failwith "never"

[<JSEmitInline("{0}.getMilliseconds()")>]
let Millisecond(d: DateTime): int = failwith "never"

[<JSEmitInline("{0}.getMinutes()")>]
let Minute(d: DateTime): int = failwith "never"

[<JSEmitInline("({0}.getMonth() + 1)")>]
let Month(d: DateTime): int = failwith "never"

[<JSEmitInline("{0}.getSeconds()")>]
let Second(d: DateTime): int = failwith "never"

[<JSEmitInline("{0}.getFullYear()")>]
let Year(d: DateTime): int = failwith "never"