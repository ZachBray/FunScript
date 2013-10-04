module internal FunScript.OtherMappings

open AST
open Microsoft.FSharp.Quotations
open System.IO

let components = 
   [
      [
         // TODO: Add support for constructors to 
         //       ExpressionReplacer.createTypeMethodMappings(...)
         ExpressionReplacer.createUnsafe 
            <@ fun () -> new StringWriter() @> 
            <@ fun () -> Core.StringWriter.StringWriter.Create() @>

      ]

      ExpressionReplacer.createTypeMethodMappings 
         typeof<StringWriter>
         typeof<Core.StringWriter.StringWriter>

   ] |> List.concat