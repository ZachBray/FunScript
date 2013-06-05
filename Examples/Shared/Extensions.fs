namespace FunScript

// ----------------------------------------------------------------------------
// Useful extensions
// ----------------------------------------------------------------------------

[<AutoOpen>]
module FunScriptExtensions = 

  [<JS; JSEmit("return {0}*1.0;")>]
  let number (a:obj) : float = failwith "never"

  [<JS; JSEmit("return Math.floor({0});")>]
  let floor (n:int) = n

  [<JS; JSEmit("return new {0}({1});")>]
  let clone (obj : 'T, args : 'A) : 'T = failwith "never"
    
  type System.Array with
    [<JS; JSEmit("return {0}.push({1});")>]
    member x.push(element:obj) = ()