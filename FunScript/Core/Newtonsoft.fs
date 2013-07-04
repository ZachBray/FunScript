[<FunScript.JS>]
module FunScript.Core.Newtonsoft
open FunScript
open System

type JsonTextWriter(sw : System.IO.StringWriter) =
   let mutable wasLastCharEndOfValue = false
   let write(x:obj) = 
      if wasLastCharEndOfValue then sw.Write ","
      sw.Write x
      wasLastCharEndOfValue <- false
   let writeValue(x : obj) = 
      write x
      wasLastCharEndOfValue <- true
   let writeEndBlock(x : obj) =
      sw.Write x
      wasLastCharEndOfValue <- true
   
   member __.WriteNull() = writeValue "null"

   //member __.WriteValue(x : DateTime) = writeValue x
   member __.WriteValue(x : byte) = writeValue x
   member __.WriteValue(x : int16) = writeValue x
   member __.WriteValue(x : int) = writeValue x
   member __.WriteValue(x : int64) = writeValue x
   member __.WriteValue(x : uint16) = writeValue x
   member __.WriteValue(x : uint32) = writeValue x
   member __.WriteValue(x : uint64) = writeValue x
   member __.WriteValue(x : float) = writeValue x
   member __.WriteValue(x : float32) = writeValue x
   member __.WriteValue(x : bool) = writeValue x
   member __.WriteValue(x : string) = writeValue("\"" + x + "\"")
   //member __.WriteValue(x : Guid) = writeValue("\"" + x.ToString() + "\"")

   member __.WriteStartObject() = write "{"
   member __.WritePropertyName x = write("\"" + x + "\":")
   member __.WriteEndObject() = writeEndBlock "}"

   member __.WriteStartArray() = write "["
   member __.WriteEndArray() = writeEndBlock "]"

   static member Create sw = JsonTextWriter(sw)


[<JSEmit("return eval(\"(\" + {0} + \")\");")>]
let parse (str : string) : obj = failwith "never"


[<JSEmit("return {1}[{0}];")>]
let get (prop : string) (obj : obj) : obj = failwith "never"

[<JSEmit("var xs = []; for(name in {0}) { xs.push(name); }; return xs;")>]
let getProps (obj : obj) : string[] = failwith "never"

type JToken(obj : obj) =
   member __.Value<'a>() = obj :?> 'a
   member __.Values<'a>() = 
      //NOTE: only JToken is supported
      obj |> getProps |> Seq.map (fun name ->
         JToken(obj |> get name))

   static member Parse(str : string) = JToken(parse str)

   //JObject(...)
   member __.Property name = JProperty (name, obj |> get name)
   member __.Properties() = 
      obj |> getProps |> Seq.map (fun name ->
         JProperty(name, obj |> get name))

and JProperty(name : string, obj : obj) =
   member __.Name = name
   member __.Value = JToken(obj)