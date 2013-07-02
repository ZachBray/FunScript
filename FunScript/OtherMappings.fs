module internal FunScript.OtherMappings

open AST
open Microsoft.FSharp.Quotations

let components = 
   [
      [
         // TODO: Add support for constructors to 
         //       ExpressionReplacer.createTypeMethodMappings(...)
         ExpressionReplacer.createUnsafe 
            <@ fun () -> new System.IO.StringWriter() @> 
            <@ fun () -> Core.StringWriter.StringWriter.Create() @>
      ]

      ExpressionReplacer.createTypeMethodMappings 
         typeof<System.IO.StringWriter>
         typeof<Core.StringWriter.StringWriter>
   ] |> List.concat