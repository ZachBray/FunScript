[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Newtonsoft

open NUnit.Framework
open System.IO
open Newtonsoft.Json
open Newtonsoft.Json.Linq

[<Test>]
let ``JsonTextWriter should emit the same for objects``() =
   check 
      <@@
         let sw = new StringWriter()
         let jw = new JsonTextWriter(sw)
         jw.WriteStartObject()
         jw.WritePropertyName "my_string_property"
         jw.WriteValue "my_value"
         jw.WritePropertyName "my_float_property"
         jw.WriteValue 1.2
         jw.WritePropertyName "my_bool_property"
         jw.WriteValue true
         jw.WriteEndObject()
         sw.ToString()
      @@>

[<Test>]
let ``JsonTextWriter should emit the same for arrays``() =
   check 
      <@@
         let sw = new StringWriter()
         let jw = new JsonTextWriter(sw)
         jw.WriteStartArray()
         jw.WriteValue 1.1
         jw.WriteValue 1.2
         jw.WriteValue 1.3
         jw.WriteValue 1.4
         jw.WriteValue 1.5
         jw.WriteEndArray()
         sw.ToString()
      @@>


[<Test>]
let ``JToken.Parse, JToken.Values and JToken.Value should work the same on JSON objects``() =
   check 
      <@@
         let token = JToken.Parse "[\"blah1\",\"blah2\",\"blah3\"]"
         token.Values<JToken>()
         |> Seq.map (fun t -> t.Value<string>())
         |> String.concat ", "
      @@>

[<Test>]
let ``JObject.Properties should work the same on JSON objects``() =
   check 
      <@@
         let token = JToken.Parse "{\"my_field\" : \"blah\"}"
         let obj = token :?> JObject
         obj.Properties()
         |> Seq.map (fun t -> t.Name + ":" + t.Value.Value<string>())
         |> String.concat ", "
      @@>