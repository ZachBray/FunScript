[<FunJS.JS>]
module FunJS.Core.IntrinsicFunctions

open FunJS

[<JSEmit("return {0};")>]
let UnboxGeneric (x:obj) :'a = failwith "never"