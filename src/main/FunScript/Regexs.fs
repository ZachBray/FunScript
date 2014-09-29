module internal FunScript.Regexs

open AST
open Microsoft.FSharp.Quotations
open System.Text.RegularExpressions

let private toSeq =
    CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
        function
        | Patterns.Coerce(expr, t) 
            when t.Name = typeof<seq<obj>>.Name || t.Name = typeof<System.Collections.IEnumerable>.Name ->
            match expr with
            | expr when expr.Type.Name = typeof<GroupCollection>.Name ->
                let mi, _ = Quote.toMethodInfoFromLambdas <@@ Core.Regex.GroupCollection.ToSeq @@>
                compiler.Compile returnStrategy (ExpressionReplacer.buildCall mi [expr])
            | expr when expr.Type.Name = typeof<MatchCollection>.Name ->
                let mi, _ = Quote.toMethodInfoFromLambdas <@@ Core.Regex.MatchCollection.ToSeq @@>
                compiler.Compile returnStrategy (ExpressionReplacer.buildCall mi [expr])
            | _ -> [] 
        | _ -> []

let components = 
  [
    [
        toSeq
        ExpressionReplacer.createUnsafe <@ fun (r) -> Regex(r) @> <@ fun r -> Core.Regex.Create(r) @>
        ExpressionReplacer.createUnsafe <@ fun (r, o) -> Regex(r, o) @> <@ Core.Regex.CreateWithOptions @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex) -> r.Options @> <@ Core.Regex.GetOptions @>

        ExpressionReplacer.createUnsafe <@ fun (s) -> Regex.Escape(s) @> <@ Core.Regex.Escape @>
        ExpressionReplacer.createUnsafe <@ fun (s) -> Regex.Unescape(s) @> <@ Core.Regex.Unescape @>

        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s) -> r.IsMatch(s) @> <@ Core.Regex.IsMatch @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, i) -> r.IsMatch(s, i) @> <@ Core.Regex.IsMatchWithOffset @>
        ExpressionReplacer.createUnsafe <@ fun (s, r) -> Regex.IsMatch(s, r) @> <@ Core.Regex.IsMatchStatic @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, o) -> Regex.IsMatch(s, r, o) @> <@ Core.Regex.IsMatchStaticWithOptions @>

        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s) -> r.Match(s) @> <@ Core.Regex.MatchFirst @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, i) -> r.Match(s, i) @> <@ Core.Regex.MatchFirstWithOffset @>
        ExpressionReplacer.createUnsafe <@ fun (s, r) -> Regex.Match(s, r) @> <@ Core.Regex.MatchFirstStatic @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, o) -> Regex.Match(s, r, o) @> <@ Core.Regex.MatchFirstStaticWithOptions @>

        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s) -> r.Matches(s) @> <@ Core.Regex.Matches @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, i) -> r.Matches(s, i) @> <@ Core.Regex.MatchesWithOffset @>
        ExpressionReplacer.createUnsafe <@ fun (s, r) -> Regex.Matches(s, r) @> <@ Core.Regex.MatchesStatic @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, o) -> Regex.Matches(s, r, o) @> <@ Core.Regex.MatchesStaticWithOptions @>

        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s) -> r.Split(s) @> <@ Core.Regex.Split @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, count) -> r.Split(s, count) @> <@ Core.Regex.SplitWithLimit @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, count, startat) -> r.Split(s, count, startat) @> <@ Core.Regex.SplitWithLimitAndOffset @>
        ExpressionReplacer.createUnsafe <@ fun (s, r) -> Regex.Split(s, r) @> <@ Core.Regex.SplitStatic @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, o) -> Regex.Split(s, r, o) @> <@ Core.Regex.SplitStaticWithOptions @>

        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, rep) -> r.Replace(s, replacement=rep) @> <@ Core.Regex.Replace @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, rep, count) -> r.Replace(s, replacement=rep, count=count) @> <@ Core.Regex.ReplaceWithLimit @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, rep, count, startat) -> r.Replace(s, replacement=rep, count=count, startat=startat) @> <@ Core.Regex.ReplaceWithLimitAndOffset @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, ev) -> r.Replace(s, evaluator=ev) @> <@ Core.Regex.ReplaceWithEvaluator @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, ev, count) -> r.Replace(s, evaluator=ev, count=count) @> <@ Core.Regex.ReplaceWithEvaluatorAndLimit @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex, s, ev, count, startat) -> r.Replace(s, evaluator=ev, count=count, startat=startat) @> <@ Core.Regex.ReplaceWithEvaluatorAndLimitAndOffset @>
        
        ExpressionReplacer.createUnsafe <@ fun (s, r, rep) -> Regex.Replace(s, r, replacement=rep) @> <@ Core.Regex.ReplaceStatic @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, rep, o) -> Regex.Replace(s, r, replacement=rep, options=o) @> <@ Core.Regex.ReplaceStaticWithOptions @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, ev) -> Regex.Replace(s, r, evaluator=ev) @> <@ Core.Regex.ReplaceStaticWithEvaluator @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, ev, o) -> Regex.Replace(s, r, evaluator=ev, options=o) @> <@ Core.Regex.ReplaceStaticWithEvaluatorAndOptions @>
    ]
    ExpressionReplacer.createTypeMethodMappings typeof<System.Text.RegularExpressions.Capture> typeof<Core.Regex.Capture>
    ExpressionReplacer.createTypeMethodMappings typeof<System.Text.RegularExpressions.Group> typeof<Core.Regex.Group>
    ExpressionReplacer.createTypeMethodMappings typeof<System.Text.RegularExpressions.Match> typeof<Core.Regex.Match>
    ExpressionReplacer.createTypeMethodMappings typeof<System.Text.RegularExpressions.GroupCollection> typeof<Core.Regex.GroupCollection>
    ExpressionReplacer.createTypeMethodMappings typeof<System.Text.RegularExpressions.MatchCollection> typeof<Core.Regex.MatchCollection>
  ] |> List.concat

