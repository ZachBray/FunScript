module internal FunScript.OtherMappings

open AST
open Microsoft.FSharp.Quotations
open System.IO
open System.Collections.Concurrent

let components = 
   [
      [
         // TODO: Add support for constructors to 
         //       ExpressionReplacer.createTypeMethodMappings(...)
         ExpressionReplacer.createUnsafe 
            <@ fun () -> new StringWriter() @> 
            <@ fun () -> Core.StringWriter.StringWriter.Create() @>

         ExpressionReplacer.createUnsafe 
            <@ Lazy.Create @> 
            <@ Core.LanguagePrimitives.Lazy<_>.Create @>

         ExpressionReplacer.createUnsafe 
            <@ Lazy.CreateFromValue @> 
            <@ Core.LanguagePrimitives.Lazy<_>.CreateFromValue @>

         ExpressionReplacer.createUnsafe 
            <@ System.Lazy.Create @> 
            <@ Core.LanguagePrimitives.Lazy.Create @>

         ExpressionReplacer.createUnsafe 
            <@ System.Lazy.CreateFromValue @> 
            <@ Core.LanguagePrimitives.Lazy.CreateFromValue @>

         ExpressionReplacer.createUnsafe 
            <@ fun () -> ConcurrentDictionary<_,_>() @> 
            <@ fun () -> Core.Dictionaries.ConcurrentDictionary<_,_>.Create() @>
      ]

      ExpressionReplacer.createTypeMethodMappings 
         typeof<StringWriter>
         typeof<Core.StringWriter.StringWriter>

      ExpressionReplacer.createTypeMethodMappings 
         typeof<ConcurrentDictionary<_,_>>
         typeof<Core.Dictionaries.ConcurrentDictionary<_,_>>

      ExpressionReplacer.createTypeMethodMappings
         typeof<System.Lazy<_>>
         typeof<Core.LanguagePrimitives.Lazy<_>>

   ] |> List.concat