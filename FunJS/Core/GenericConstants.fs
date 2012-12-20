[<FunJS.JS>]
module FunJS.Core.GenericConstants

open FunJS

[<JSEmit("return 0;")>]
let Zero<'a> :'a = failwith "never"

[<JSEmit("return 1;")>]
let One<'a> : 'a = failwith "never"