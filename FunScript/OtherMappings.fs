module internal FunScript.OtherMappings

open AST
open Microsoft.FSharp.Quotations
open System.IO
open Newtonsoft.Json

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

   ] |> List.concat