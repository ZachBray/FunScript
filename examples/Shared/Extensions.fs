namespace FunScript

// ----------------------------------------------------------------------------
// Useful extensions
// ----------------------------------------------------------------------------

[<AutoOpen>]
module FunScriptExtensions = 

  [<JS; JSEmit("return {0}*1.0;")>]
  let number (a:obj) : float = failwith "never"