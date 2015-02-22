[<FunScript.JS>]
module FunScript.Core.GenericConstants

open FunScript

[<JSEmitInline("0")>]
let Zero<'a> :'a = failwith "never"

[<JSEmitInline("1")>]
let One<'a> : 'a = failwith "never"