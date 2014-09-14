module internal FunScript.Regexs

open AST
open Microsoft.FSharp.Quotations
open System.Text.RegularExpressions


let components = 
    [
        ExpressionReplacer.createUnsafe <@ fun (r) -> Regex(r) @> <@ Core.Regex.Create @>
        ExpressionReplacer.createUnsafe <@ fun (r, o) -> Regex(r, o) @> <@ Core.Regex.CreateWithOptions @>
        ExpressionReplacer.createUnsafe <@ fun (r: Regex) -> r.Options @> <@ Core.Regex.GetOptions @>
//        ExpressionReplacer.createUnsafe <@ fun (r: Regex) -> r.ToString() @> <@ Core.Regex.ToString @>

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

        ExpressionReplacer.createUnsafe <@ fun (m: Capture) -> m.Index @>   <@ Core.Regex.Capture.Index @>
        ExpressionReplacer.createUnsafe <@ fun (m: Capture) -> m.Length @>  <@ Core.Regex.Capture.Length @>
        ExpressionReplacer.createUnsafe <@ fun (m: Capture) -> m.Value @>   <@ Core.Regex.Capture.Value @>
        ExpressionReplacer.createUnsafe <@ fun (m: Capture) -> m.ToString() @>   <@ Core.Regex.Capture.ToString @>

        ExpressionReplacer.createUnsafe <@ fun (m: Group) -> m.Success @> <@ Core.Regex.Group.Success @>

        ExpressionReplacer.createUnsafe <@ fun (m: Match) -> m.Groups @>  <@ Core.Regex.Match.Groups @>

        ExpressionReplacer.createUnsafe <@ fun (ms: MatchCollection) -> ms.Count @> <@ Core.Regex.Collection.Count @>
        ExpressionReplacer.createUnsafe <@ fun (ms: MatchCollection) -> ms.GetEnumerator() @> <@ Core.Regex.Collection.GetEnumerator @>
        ExpressionReplacer.createUnsafe <@ fun (ms: MatchCollection, index: int) -> ms.Item(index) @> <@ Core.Regex.Collection.Item @>

        ExpressionReplacer.createUnsafe <@ fun (ms: GroupCollection) -> ms.Count @> <@ Core.Regex.Collection.Count @>
        ExpressionReplacer.createUnsafe <@ fun (ms: GroupCollection) -> ms.GetEnumerator() @> <@ Core.Regex.Collection.GetEnumerator @>
        ExpressionReplacer.createUnsafe <@ fun (ms: GroupCollection, index: int) -> ms.Item(index) @> <@ Core.Regex.Collection.Item @>
    ]

