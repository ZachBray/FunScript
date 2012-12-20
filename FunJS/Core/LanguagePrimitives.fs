[<FunJS.JS>]
module FunJS.Core.LanguagePrimitives

open FunJS
open System.Collections.Generic

[<JSEmit("return {0};")>]
let UnboxGeneric (x:obj) :'a = failwith "never"

type GenericComparer<'a when 'a: comparison>() =
   interface IComparer<'a> with
      member __.Compare(x, y) = compare x y


type KeyValuePair<'Key, 'Value>(key, value) =
   member __.Key: 'Key = key
   member __.Value: 'Value = value