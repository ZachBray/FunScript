module internal FunScript.OtherMappings

open AST
open Microsoft.FSharp.Quotations
open System.IO
open Newtonsoft.Json
open Newtonsoft.Json.Linq

let components = 
   [
      [
         // TODO: Add support for constructors to 
         //       ExpressionReplacer.createTypeMethodMappings(...)
         ExpressionReplacer.createUnsafe 
            <@ fun () -> new StringWriter() @> 
            <@ fun () -> Core.StringWriter.StringWriter.Create() @>

         ExpressionReplacer.createUnsafe 
            <@ fun tw -> new JsonTextWriter(tw) @> 
            <@ fun tw -> Core.Newtonsoft.JsonTextWriter.Create(tw) @>

         ExpressionReplacer.createUnsafe 
            <@ fun (jt : JProperty) -> jt.Value @> 
            <@ fun (jt : Core.Newtonsoft.JProperty) -> jt.Value @> 

         ExpressionReplacer.createUnsafe 
            <@ fun (jt : JToken) -> jt.Value<_>() @> 
            <@ fun (jt : Core.Newtonsoft.JToken) -> jt.Value<_>() @> 

      ]

      ExpressionReplacer.createTypeMethodMappings 
         typeof<StringWriter>
         typeof<Core.StringWriter.StringWriter>

      
      ExpressionReplacer.createTypeMethodMappings 
         typeof<JsonTextWriter>
         typeof<Core.Newtonsoft.JsonTextWriter>

      ExpressionReplacer.createTypeMethodMappings 
         typeof<JsonWriter>
         typeof<Core.Newtonsoft.JsonTextWriter>

      ExpressionReplacer.createTypeMethodMappings 
         typeof<JToken>
         typeof<Core.Newtonsoft.JToken>

      ExpressionReplacer.createTypeMethodMappings 
         typeof<JObject>
         typeof<Core.Newtonsoft.JToken>

      ExpressionReplacer.createTypeMethodMappings 
         typeof<JProperty>
         typeof<Core.Newtonsoft.JProperty>

   ] |> List.concat