module internal FunScript.Dictionaries

open AST
open Microsoft.FSharp.Quotations
open System.Collections.Generic

let components = 
    [
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>) -> xs.GetEnumerator() @> <@ Core.Dictionaries.MutableDic.GetEnumerator @>
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>.KeyCollection) -> xs.GetEnumerator() @> <@ Core.Dictionaries.MutableDic.GetEnumerator @>
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>.ValueCollection) -> xs.GetEnumerator() @> <@ Core.Dictionaries.MutableDic.GetEnumerator @>

        ExpressionReplacer.createUnsafe <@ fun () -> Dictionary<_,_>() @> <@ Core.Dictionaries.MutableDic.Create @>
        ExpressionReplacer.createUnsafe <@ fun (size: int) -> Dictionary<_,_>(size) @> <@ Core.Dictionaries.MutableDic.CreateWithSize @>

        ExpressionReplacer.createUnsafe <@ fun (xs: seq<_*_>) -> dict xs @> <@ Core.Dictionaries.MutableDic.OfSeq @>
        ExpressionReplacer.createUnsafe <@ fun (xs: IDictionary<_,_>) -> Dictionary<_,_>(xs) @> <@ Core.Dictionaries.MutableDic.OfIDictionary @>

        // Interface members
        ExpressionReplacer.createUnsafe <@ fun (xs: IDictionary<_,_>) -> xs.Count @> <@ Core.Dictionaries.MutableDic.EnumCount @>
        ExpressionReplacer.createUnsafe <@ fun (xs: IDictionary<_,_>) -> xs.IsReadOnly @> <@ Core.Dictionaries.MutableDic.IsReadOnly @>
        ExpressionReplacer.createUnsafe <@ fun (xs: IDictionary<_,_>, key: _) -> xs.Item(key) @> <@ Core.Dictionaries.MutableDic.GetItem @>
        ExpressionReplacer.createUnsafe <@ fun (xs: IDictionary<_,_>) -> xs.Keys @> <@ Core.Dictionaries.MutableDic.Keys @>
        ExpressionReplacer.createUnsafe <@ fun (xs: IDictionary<_,_>) -> xs.Values @> <@ Core.Dictionaries.MutableDic.Values @>
        ExpressionReplacer.createUnsafe <@ fun (xs: IDictionary<_,_>, key: _) -> xs.ContainsKey(key) @> <@ Core.Dictionaries.MutableDic.ContainsKey @>

        // Implementation members
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>) -> xs.Count @> <@ Core.Dictionaries.MutableDic.DicCount @>
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>, key: _) -> xs.Item(key) @> <@ Core.Dictionaries.MutableDic.GetItem @>
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>, key: _, value: _) -> xs.Item(key) <- value @> <@ Core.Dictionaries.MutableDic.SetItem @>
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>) -> xs.Keys @> <@ Core.Dictionaries.MutableDic.Keys @>
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>) -> xs.Values @> <@ Core.Dictionaries.MutableDic.Values @>

        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>, key: _, value: _) -> xs.Add(key, value) @> <@ Core.Dictionaries.MutableDic.Add @>
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>) -> xs.Clear() @> <@ Core.Dictionaries.MutableDic.Clear @>
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>, key: _) -> xs.ContainsKey(key) @> <@ Core.Dictionaries.MutableDic.ContainsKey @>
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>, value: _) -> xs.ContainsValue(value) @> <@ Core.Dictionaries.MutableDic.ContainsValue @> 
        ExpressionReplacer.createUnsafe <@ fun (xs: Dictionary<_,_>, key: _) -> xs.Remove(key) @> <@ Core.Dictionaries.MutableDic.Remove @>
    ]
