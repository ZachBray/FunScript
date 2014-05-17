[<FunScript.JS>]
module FunScript.Core.StringWriter
open FunScript

[<JSEmit("{0}.toString = {1}; return {0};")>]
let populateToString (x : 'a, f : unit -> string) : 'a = 
   failwith "never"

type StringWriter() =
   let mutable x = ""
   member __.Print() = x
   member __.Write(str : obj) = x <- x + (str.ToString())
   static member Create() = 
      let x = StringWriter()
      populateToString(x, x.Print)