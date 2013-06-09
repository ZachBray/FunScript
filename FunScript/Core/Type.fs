[<FunScript.JS>]
module FunScript.Core.Type

type Type(name : string) =
   member __.Name = name


let create name =
   Type(name)