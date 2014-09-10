module internal FunScript.DateTimes

open AST
open Microsoft.FSharp.Quotations
open System

let components = 
    [
        ExpressionReplacer.createUnsafe <@ fun (y, m, d) -> DateTime(y, m, d) @> <@ Core.DateTime.CreateYMD @>
        ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s) -> DateTime(y, m, d, h, min, s) @> <@ Core.DateTime.CreateYMDHMS @>
        ExpressionReplacer.createUnsafe <@ fun (y, m, d, h, min, s, ms: int) -> DateTime(y, m, d, h, min, s, ms) @> <@ Core.DateTime.CreateYMDHMSM @>

        ExpressionReplacer.createUnsafe <@ fun () -> DateTime.Now @> <@ Core.DateTime.Now @>
        ExpressionReplacer.createUnsafe <@ fun (s) -> DateTime.Parse(s) @> <@ Core.DateTime.Parse @>

        ExpressionReplacer.createUnsafe <@ fun (d: DateTime) -> d.Day @> <@ Core.DateTime.Day @>
        ExpressionReplacer.createUnsafe <@ fun (d: DateTime) -> d.DayOfWeek @> <@ Core.DateTime.DayOfWeek @>
        ExpressionReplacer.createUnsafe <@ fun (d: DateTime) -> d.Hour @> <@ Core.DateTime.Hour @>        
        ExpressionReplacer.createUnsafe <@ fun (d: DateTime) -> d.Millisecond @> <@ Core.DateTime.Millisecond @>
        ExpressionReplacer.createUnsafe <@ fun (d: DateTime) -> d.Minute @> <@ Core.DateTime.Minute @>
        ExpressionReplacer.createUnsafe <@ fun (d: DateTime) -> d.Month @> <@ Core.DateTime.Month @>        
        ExpressionReplacer.createUnsafe <@ fun (d: DateTime) -> d.Second @> <@ Core.DateTime.Second @>
        ExpressionReplacer.createUnsafe <@ fun (d: DateTime) -> d.Year @> <@ Core.DateTime.Year @>
    ]


