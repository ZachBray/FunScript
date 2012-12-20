// --------------------------------------------------------------------------------------
// Mappings for the JSON Type Provider runtime
// --------------------------------------------------------------------------------------

module private FunJS.Data.JsonProvider

open FunJS
open FSharp.Data.Json
open ProviderImplementation

// --------------------------------------------------------------------------------------
// Low level JS helpers for getting types & reflection

[<AutoOpen>]
module JsHelpers = 
  [<JS; JSEmit("return typeof({0});")>]
  let typeof(o:obj) : string = failwith "never"
  [<JS; JSEmit("return Array.isArray({0});")>]
  let isArray(o:obj) : bool = failwith "never"
  [<JS; JSEmit("return {0}==null;")>]
  let isNull(o:obj) : bool = failwith "never"
  [<JS; JSEmit("return typeof({0}[{1}]) != \"undefined\";")>]
  let hasProperty(o:obj,name:string) : bool = failwith "never"

// --------------------------------------------------------------------------------------
// Reimplementation of the Json Type Provider runtime 

[<JS>]
type JsRuntime = 
  // Document is parsed into JS object using nasty 'eval'
  // Various getters that do conversions become just identities

  [<JSEmit("var it = {0}; return (typeof(it)==\"string\") ? eval(\"(function(a){return a;})(\" + it + \")\") : it;")>]
  static member Parse(source:string) : JsonValue = failwith "never"

  [<JSEmit("return {0};")>]
  static member CreateDocument(json:JsonValue) : JsonDocument = failwith "never"

  [<JSEmit("return {0}[{1}];")>]
  static member GetProperty(json:JsonValue, name:string) : JsonValue = failwith "never"

  [<JSEmit("return {0};")>]
  static member Identity(arg:obj) : obj = failwith "never"

  // The `GetArrayChildrenByTypeTag` is a JS reimplementation of the
  // equivalent in the JSON type provider. The following functions
  // are derived (copied from JSON provider code)
  static member GetArrayChildrenByTypeTag<'P, 'R>
    (doc:JsonValue, tag:string, pack:JsonValue -> 'P, f:'P -> 'R) : 'R[] =
    let matchesTag (value:obj) = 
      if isNull value then false
      elif typeof(value) = "boolean" then tag = "Boolean"
      elif typeof(value) = "number" then tag = "Number"
      elif typeof(value) = "string" then tag = "string"
      elif isArray(value) then tag = "Array"
      elif typeof(value) = "object" then tag = "Record"
      else failwith "JSON mismatch: Unexpected node type" 
    if isArray doc then
      doc |> unbox 
          |> Array.filter matchesTag
          |> Array.map (pack >> f)
    else failwith "JSON mismatch: Expected Array node"

  static member GetArrayChildByTypeTag
    (value:JsonValue, tag:string) : JsonValue = 
    let arr = JsRuntime.GetArrayChildrenByTypeTag(value, tag, id, id) 
    if arr.Length = 1 then arr.[0] 
    else failwith "JSON mismatch: Expected single value, but found multiple."

  static member TryGetArrayChildByTypeTag<'P, 'R>
    (doc:JsonValue, tag:string, pack:JsonValue-> 'P, f:'P -> 'R) : 'R option = 
    let arr = JsRuntime.GetArrayChildrenByTypeTag(doc, tag, pack, f) 
    if arr.Length = 1 then Some arr.[0]
    elif arr.Length = 0 then None
    else failwith "JSON mismatch: Expected Array with single or no elements."

  static member TryGetValueByTypeTag<'P, 'R>
    (value:JsonValue, tag:string, pack:JsonValue -> 'P, f:'P -> 'R) : 'R option =
    let arrayValue = JsonValue.Array [value]
    JsonOperations.TryGetArrayChildByTypeTag(arrayValue, tag, pack, f) 

  static member ConvertOptionalProperty<'P, 'R>
    (doc:JsonValue, name:string, packer:JsonValue -> 'P, f:'P -> 'R) : 'R option = 
    if hasProperty(doc, name) then Some (f (packer (JsRuntime.GetProperty(doc, name))))
    else None

  // In the dynamic world, this does not do anything at all :-)  
  [<JSEmit("return {0};")>]
  static member ConvertArray<'P, 'R>
    (value:JsonValue, packer:JsonValue -> 'P, f:'P -> 'R) : 'R[] = failwith "never"

// --------------------------------------------------------------------------------------
// Define the mappings

let components = 
  [ // Document is parsed into JS object
    ExpressionReplacer.create <@ JsonValue.Parse @> <@ JsRuntime.Parse @>
    ExpressionReplacer.create <@ JsonDocument.Create @> <@ JsRuntime.CreateDocument @>
    ExpressionReplacer.createUnsafe <@ fun (d:JsonDocument) -> d.JsonValue @> <@ JsRuntime.Identity @>

    // Get property using indexer
    ExpressionReplacer.createUnsafe <@ JsonOperations.GetProperty @> <@ JsRuntime.GetProperty @>

    // Conversion becomes just identity
    ExpressionReplacer.createUnsafe <@ JsonOperations.GetString @> <@ JsRuntime.Identity @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.GetBoolean @> <@ JsRuntime.Identity @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.GetFloat @> <@ JsRuntime.Identity @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.GetDecimal @> <@ JsRuntime.Identity @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.GetInteger @> <@ JsRuntime.Identity @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.GetInteger64 @> <@ JsRuntime.Identity @>

    // Functions that do something tricky are reimplemented
    ExpressionReplacer.createUnsafe <@ JsonOperations.GetArrayChildrenByTypeTag @> <@ JsRuntime.GetArrayChildrenByTypeTag @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.ConvertArray @> <@ JsRuntime.ConvertArray @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.ConvertOptionalProperty @> <@ JsRuntime.ConvertOptionalProperty @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.TryGetArrayChildByTypeTag @> <@ JsRuntime.TryGetArrayChildByTypeTag @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.GetArrayChildByTypeTag @> <@ JsRuntime.GetArrayChildByTypeTag @>
    ExpressionReplacer.createUnsafe <@ JsonOperations.TryGetValueByTypeTag @> <@ JsRuntime.TryGetValueByTypeTag @>
  ]
