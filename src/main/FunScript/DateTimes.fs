module internal FunScript.DateTimes

open AST
open System
open Microsoft.FSharp.Quotations

let components = 
    [
        [
            ExpressionReplacer.createUnsafe <@ fun (y, m, d) -> DateTime(y, m, d) @> <@ Core.DateTime.DateTime.FromYMD @>
            ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s) -> DateTime(y, m, d, h, min, s) @> <@ Core.DateTime.DateTime.FromYMDHMS @>
            ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s, kind: DateTimeKind) -> DateTime(y, m, d, h, min, s, kind) @> <@ Core.DateTime.DateTime.FromYMDHMSwithKind @>
            ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s, ms: int) -> DateTime(y, m, d, h, min, s, ms) @> <@ Core.DateTime.DateTime.FromYMDHMSM @>
            ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s, ms: int, kind: DateTimeKind) -> DateTime(y, m, d, h, min, s, ms, kind) @> <@ Core.DateTime.DateTime.FromYMDHMSMwithKind @>

            ExpressionReplacer.createUnsafe <@ fun (t) -> TimeSpan(t) @> <@ Core.DateTime.TimeSpan.FromTicks @>
            ExpressionReplacer.createUnsafe <@ fun (h,m,s) -> TimeSpan(h,m,s) @> <@ Core.DateTime.TimeSpan.FromHMS @>
            ExpressionReplacer.createUnsafe <@ fun (d,h,m,s) -> TimeSpan(d,h,m,s) @> <@ Core.DateTime.TimeSpan.FromDHMS @>
            ExpressionReplacer.createUnsafe <@ fun (d,h,m,s,ms) -> TimeSpan(d,h,m,s,ms) @> <@ Core.DateTime.TimeSpan.FromDHMSM @>
        ]

        ExpressionReplacer.createTypeMethodMappings
            typeof<System.TimeSpan>
            typeof<Core.DateTime.TimeSpan>

        ExpressionReplacer.createTypeMethodMappings
            typeof<System.DateTime>
            typeof<Core.DateTime.DateTime>

    ] |> List.concat
