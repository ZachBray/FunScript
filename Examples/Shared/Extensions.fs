namespace FunJS

// ----------------------------------------------------------------------------
// Useful extensions
// ----------------------------------------------------------------------------

[<AutoOpen>]
module FunJSExtensions = 

  [<JS; JSEmit("return {0}*1.0;")>]
  let number (a:obj) : float = failwith "never"

  [<JS; JSEmit("return Math.floor({0});")>]
  let floor (n:int) = n

  type System.Array with
    [<JS; JSEmit("return {0}.push({1});")>]
    member x.push(element:obj) = ()