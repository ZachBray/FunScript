[<FunScript.JS>]
module FunScript.Core.Dictionaries

open System
open FunScript

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