module internal FunScript.Regexs

open System.Text.RegularExpressions

let components = 
    [
        ExpressionReplacer.createUnsafe <@ fun (s, r) -> Regex.IsMatch(s, r) @> <@ Core.Regex.IsMatch @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, o) -> Regex.IsMatch(s, r, o) @> <@ Core.Regex.IsMatchWithOptions @>
        ExpressionReplacer.createUnsafe <@ fun (s, r) -> Regex.Match(s, r) @> <@ Core.Regex.MatchFirst @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, o) -> Regex.Match(s, r, o) @> <@ Core.Regex.MatchFirstWithOptions @>
        ExpressionReplacer.createUnsafe <@ fun (s, r) -> Regex.Matches(s, r) @> <@ Core.Regex.Matches @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, o) -> Regex.Matches(s, r, o) @> <@ Core.Regex.MatchesWithOptions @>

        ExpressionReplacer.createUnsafe <@ fun (s, r) -> Regex.Split(s, r) @> <@ Core.Regex.Split @>
        ExpressionReplacer.createUnsafe <@ fun (s, r, o) -> Regex.Split(s, r, o) @> <@ Core.Regex.SplitWithOptions @>
        ExpressionReplacer.createUnsafe <@ fun (s, p, r) -> Regex.Replace(s, p, replacement=r) @> <@ Core.Regex.Replace @>
        ExpressionReplacer.createUnsafe <@ fun (s, p, r, o) -> Regex.Replace(s, p, replacement=r, options=o) @> <@ Core.Regex.ReplaceWithOptions @>

        ExpressionReplacer.createUnsafe <@ fun (m: Match) -> m.Groups @>  <@ Core.Regex.Match.Groups @>
        ExpressionReplacer.createUnsafe <@ fun (m: Match) -> m.Index @>   <@ Core.Regex.Match.Index @>
        ExpressionReplacer.createUnsafe <@ fun (m: Match) -> m.Length @>  <@ Core.Regex.Match.Length @>
        ExpressionReplacer.createUnsafe <@ fun (m: Match) -> m.Success @> <@ Core.Regex.Match.Success @>
        ExpressionReplacer.createUnsafe <@ fun (m: Match) -> m.Value @>   <@ Core.Regex.Match.Value @>

        ExpressionReplacer.createUnsafe <@ fun (ms: MatchCollection) -> ms.Count @> <@ Core.Regex.MatchCollection.Count @>
        ExpressionReplacer.createUnsafe <@ fun (ms: MatchCollection) -> ms.GetEnumerator @> <@ Core.Regex.MatchCollection.GetEnumerator @>
        ExpressionReplacer.createUnsafe <@ fun (ms: MatchCollection, index: int) -> ms.Item(index) @> <@ Core.Regex.MatchCollection.Item @>

        ExpressionReplacer.createUnsafe <@ fun (m: Group) -> m.Length @>  <@ Core.Regex.Group.Length @>
        ExpressionReplacer.createUnsafe <@ fun (m: Group) -> m.Value @>   <@ Core.Regex.Group.Value @>

        ExpressionReplacer.createUnsafe <@ fun (ms: GroupCollection) -> ms.Count @> <@ Core.Regex.GroupCollection.Count @>
        ExpressionReplacer.createUnsafe <@ fun (ms: GroupCollection) -> ms.GetEnumerator @> <@ Core.Regex.GroupCollection.GetEnumerator @>
        ExpressionReplacer.createUnsafe <@ fun (ms: GroupCollection, index: int) -> ms.Item(index) @> <@ Core.Regex.GroupCollection.Item @>
    ]

