[<FunScript.JS>]
module FunScript.Core.LanguagePrimitives

open FunScript
open System.Collections.Generic

[<JSEmitInline("{0}")>]
let UnboxGeneric (x:obj) :'a = failwith "never"

[<JSEmitInline("{0}")>]
let UnboxFast (x:obj) :'a = failwith "never"

type GenericComparer<'a when 'a: comparison>() =
   interface IComparer<'a> with
      member __.Compare(x, y) = compare x y

type KeyValuePair<'Key, 'Value>(key, value) =
   member __.Key: 'Key = key
   member __.Value: 'Value = value

type Lazy<'T>(value : 'T, factory: unit -> 'T) = 

    let mutable isCreated = false
    let mutable value = value 

    static member Create(f: (unit->'T)) : Lazy<'T> = 
        Lazy<'T> (value = Unchecked.defaultof<'T>, factory = f)
    static member CreateFromValue(x:'T) : Lazy<'T> = 
        Lazy<'T> (value = x, factory = fun () -> x)
    member x.IsValueCreated = isCreated
    member x.Value =  
        if not isCreated then
            value <- factory()
            isCreated <- true
        value

type Exception(message) =
    member __.Message = message