[<FunScript.JS>]
module FunScript.Core.Dictionaries

open System
open FunScript
open System.Collections.Generic


[<JSEmitInline("{0}[{1}]")>]
let private getValue(dic: obj) (key: obj): obj = failwith "never"

[<JSEmitInline("Object.keys({0})")>]
let private getKeys(dic: obj): obj[] = failwith "never"

module MutableDic =
    let CollToSeq(coll: ICollection<'a>) =
        let array = unbox coll: 'a[]
        Seq.unfold (fun i -> if i < array.Length then Some(array.[i], i + 1) else None) 0

    let KeysToSeq(_keys: Dictionary<'k,'v>.KeyCollection) =
        let keys = unbox _keys: string[]
        Seq.unfold (fun i -> if i < keys.Length then Some(keys.[i], i + 1) else None) 0

    let ValuesToSeq(_values: Dictionary<'k,'v>.ValueCollection) =
        let values = unbox _values: 'v[]
        Seq.unfold (fun i -> if i < values.Length then Some(values.[i], i + 1) else None) 0

    let DicToSeq(dic: IDictionary<'k,'v>) =
        let keys = getKeys(dic)
        0 |> Seq.unfold (fun i ->
            if i < keys.Length
            then let k = keys.[i] in Some(LanguagePrimitives.KeyValuePair(k, getValue dic k), i + 1)
            else None)

    [<JSEmitInline("Object.keys({0}).length")>]    
    let DicCount(dic: Dictionary<'k,'v>): int = failwith "never"

    [<JSEmitInline("Object.keys({0}).length")>]
    let EnumCount(dic: seq<_>): int = failwith "never"

    [<JSEmitInline("{0}.length")>]
    let KeysCount(keys: Dictionary<'k,'v>.KeyCollection): int = failwith "never"
    
    [<JSEmitInline("{0}.length")>]
    let ValuesCount(values: Dictionary<'k,'v>.ValueCollection): int = failwith "never"

    [<JSEmitInline("{}")>]
    let Create(): Dictionary<'k,'v> = failwith "never"

    // NOTE: Just ignore the initial capacity
    let CreateWithSize(size: int) = Create()

    // NOTE: This property is only exposed by the IDictionary interface so it's safe to return always true
    [<JSEmitInline("true")>]
    let IsReadOnly(dic: seq<_>): bool = failwith "never"

    [<JSEmitInline("{0}[{1}]")>]
    let GetItem(dic: Dictionary<'k,'v>, key: 'k): 'v = failwith "never"

    [<JSEmit("if ({0}[{1}] !== undefined) { {0}[{1}] = {2} } else { throw 'Key not found' }")>]
    let SetItem(dic: Dictionary<'k,'v>, key: 'k, value: 'v): unit = failwith "never"

    [<JSEmitInline("Object.keys({0})")>]
    let Keys(dic: Dictionary<'k,'v>): Dictionary<'k,'v>.KeyCollection = failwith "never"

    [<JSEmit("var values = []; for (var k in {0}) { values.push({0}[k]) } return values")>]
    let Values(dic: Dictionary<'k,'v>): Dictionary<'k,'v>.ValueCollection = failwith "never"

    [<JSEmit("if ({0}[{1}] === undefined) { {0}[{1}] = {2} } else { throw 'Key already exists' }")>]
    let Add(dic: Dictionary<'k,'v>, key: 'k, value: 'v): unit = failwith "never"

    let OfSeq(keyValuePairs: seq<'k*'v>) =
        let dic = Create()
        for key, value in keyValuePairs do
            dic.Add(key, value)
        dic

    // TODO: This is better to avoid overheading, but it may cause unexpected behaviour if we keep using the interface. Fix?
    [<JSEmitInline("{0}")>]
    let OfIDictionary(dic: IDictionary<'k,'v>): Dictionary<'k,'v> = failwith "never"

    [<JSEmitInline("({0} = {})")>]
    let Clear(dic: Dictionary<'k,'v>): unit = failwith "never"

    [<JSEmitInline("({0}[{1}] !== undefined)")>]
    let ContainsKey(dic: Dictionary<'k,'v>, key: 'k): bool = failwith "never"

    [<JSEmit("for (var key in {0}) { if ({0}[key] === {1}) { return true } } return false")>]
    let ContainsValue(dic: Dictionary<'k,'v>, value: 'v): bool = failwith "never"

    [<JSEmit("if ({0}[{1}] !== undefined) { delete {0}[{1}]; return true; } else { return false; }")>]
    let Remove(dic: Dictionary<'k,'v>, key: 'k): bool = failwith "never"


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