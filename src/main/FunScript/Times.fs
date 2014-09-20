module internal FunScript.Times

open AST
open System
open Microsoft.FSharp.Quotations

let components = 
    [
        [
            ExpressionReplacer.createUnsafe <@ fun (y, m, d) -> DateTime(y, m, d) @> <@ Core.Time.DateTime.FromYMD @>
            ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s) -> DateTime(y, m, d, h, min, s) @> <@ Core.Time.DateTime.FromYMDHMS @>
            ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s, kind: DateTimeKind) -> DateTime(y, m, d, h, min, s, kind) @> <@ Core.Time.DateTime.FromYMDHMSwithKind @>
            ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s, ms: int) -> DateTime(y, m, d, h, min, s, ms) @> <@ Core.Time.DateTime.FromYMDHMSM @>
            ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s, ms: int, kind: DateTimeKind) -> DateTime(y, m, d, h, min, s, ms, kind) @> <@ Core.Time.DateTime.FromYMDHMSMwithKind @>

            ExpressionReplacer.createUnsafe <@ fun (t) -> TimeSpan(t) @> <@ Core.Time.TimeSpan.FromTicks @>
            ExpressionReplacer.createUnsafe <@ fun (h,m,s) -> TimeSpan(h,m,s) @> <@ Core.Time.TimeSpan.FromHMS @>
            ExpressionReplacer.createUnsafe <@ fun (d,h,m,s) -> TimeSpan(d,h,m,s) @> <@ Core.Time.TimeSpan.FromDHMS @>
            ExpressionReplacer.createUnsafe <@ fun (d,h,m,s,ms) -> TimeSpan(d,h,m,s,ms) @> <@ Core.Time.TimeSpan.FromDHMSM @>
        ]

        ExpressionReplacer.createTypeMethodMappings
            typeof<System.TimeSpan>
            typeof<Core.Time.TimeSpan>

        ExpressionReplacer.createTypeMethodMappings
            typeof<System.DateTime>
            typeof<Core.Time.DateTime>

        ExpressionReplacer.createTypeMethodMappings
            typeof<System.Timers.ElapsedEventArgs>
            typeof<Core.Time.ElapsedEventArgs>

        ExpressionReplacer.createTypeMethodMappings
            typeof<System.Timers.Timer>
            typeof<Core.Time.Timer>

    ] |> List.concat
