[<FunScript.JS>]
module FunScript.Core.GenericConstants

open FunScript

[<JSEmit("return 0;")>]
let Zero<'a> :'a = failwith "never"

[<JSEmit("return 1;")>]
let One<'a> : 'a = failwith "never"