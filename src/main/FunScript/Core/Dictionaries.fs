[<FunScript.JS>]
module FunScript.Core.Dictionaries

open System
open FunScript
open System.Collections.Generic

module MutableDic =
    [<JSEmitInline("{}")>]
    let private createUnsafe(): Dictionary<'k,'v> = failwith "never"

    let Create(): Dictionary<'k,'v> =
        let dic = createUnsafe()
        FunScript.Core.Enumerator.AddDicEnumerator(dic)
        dic

    // NOTE: Just ignore the initial capacity
    let CreateWithSize(size: int) =
        Create()

    [<JSEmitInline("Object.keys({0}).length")>]
    let DicCount(dic: Dictionary<'k,'v>): int = failwith "never"

    [<JSEmitInline("Object.keys({0}).length")>]
    let EnumCount(dic: seq<_>): int = failwith "never"

    // NOTE: This property is only exposed by the IDictionary interface
    //       It's safe to return always true
    [<JSEmitInline("true")>]
    let IsReadOnly(dic: seq<_>): bool = failwith "never"

    [<JSEmitInline("{0}[{1}]")>]
    let GetItem(dic: Dictionary<'k,'v>, key: 'k): 'v = failwith "never"

    [<JSEmit("if ({0}[{1}] !== undefined) { {0}[{1}] = {2} } else { throw 'KeyNotFound' }")>]
    let SetItem(dic: Dictionary<'k,'v>, key: 'k, value: 'v): unit = failwith "never"

    [<JSEmitInline("Object.keys({0})")>]
    let private keysUnsafe(dic: Dictionary<'k,'v>): seq<'k> = failwith "never"

    let Keys(dic: Dictionary<'k,'v>) =
        let ks = keysUnsafe(dic)
        FunScript.Core.Enumerator.AddArrayEnumerator(ks)
        ks

    [<JSEmit("var keys = Object.keys({0}); var array = new Array(keys.length); for (var i = 0; i < keys.length; i++) { array[i] = {0}[keys[i]] } return array")>]
    let private valuesUnsafe(dic: Dictionary<'k,'v>): seq<'v> = failwith "never"

    let Values(dic: Dictionary<'k,'v>) =
        let vs = valuesUnsafe(dic)
        FunScript.Core.Enumerator.AddArrayEnumerator(vs)
        vs

    [<JSEmit("if ({0}[{1}] === undefined) { {0}[{1}] = {2} } else { throw 'KeyExists' }")>]
    let Add(dic: Dictionary<'k,'v>, key: 'k, value: 'v): unit = failwith "never"

    let OfSeq(keyValuePairs: seq<'k*'v>) =
        let dic = Create()
        for key, value in keyValuePairs do
            dic.Add(key, value)
        dic

    // TODO: This is better to avoid overheading, but it may cause unexpected behaviour if we keep using the interface. Fix?
    [<JSEmitInline("{0}")>]
    let OfIDictionary(dic: IDictionary<'k,'v>): Dictionary<'k,'v> = failwith "never"

    [<JSEmitInline("for (var key in {0}) { delete {0}[key] }")>]
    let Clear(dic: Dictionary<'k,'v>): unit = failwith "never"

    [<JSEmitInline("({0}[{1}] !== undefined)")>]
    let ContainsKey(dic: Dictionary<'k,'v>, key: 'k): bool = failwith "never"

    [<JSEmit("for (var key in {0}) { if ({0}[key] === {1}) { return true } } return false")>]
    let ContainsValue(dic: Dictionary<'k,'v>, value: 'v): bool = failwith "never"

    [<JSEmit("if ({0}[{1}] !== undefined) { delete {0}[{1}]; return true; } else { return false; }")>]
    let Remove(dic: Dictionary<'k,'v>, key: 'k): bool = failwith "never"

    [<JSEmitInline("{0}.GetEnumerator()")>]
    let GetEnumerator(dic: Dictionary<'k,'v>): Dictionary.Enumerator<'k, 'v> = failwith "never" 



[<JSEmit("return {0}[{1}] !== undefined;")>]
let hasProperty (x : obj) (prop : string) = failwith "never"

[<JSEmit("return {0}[{1}];")>]
let getProperty (x : obj) (prop : string) = failwith "never"

[<JSEmit("{0}[{1}] = {2};")>]
let setProperty (x : obj) (prop : string) (value : obj) = failwith "never"

let nextId = ref 0

let getUniqueObjectId(x : obj)  =
    let objId = "FunScriptObjectId"
    if hasProperty x objId then getProperty x objId
    else
        nextId := !nextId + 1
        let thisId = !nextId
        setProperty x objId thisId
        thisId

// Note: JS only has single-threaded execution.
// TODO: Use array/object for hashtable like performance
type ConcurrentDictionary<'k, 'v>() =
    let mutable innerCache = Map.empty

    member __.GetOrAdd(key : 'k, valueFactory : 'k -> 'v) =
        let id = getUniqueObjectId key
        match innerCache.TryFind id with
        | None ->
            let value = valueFactory key
            innerCache <- innerCache.Add(id, value)
            value
        | Some v -> v

    static member Create() = ConcurrentDictionary<'k, 'v>()