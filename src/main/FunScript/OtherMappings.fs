module internal FunScript.OtherMappings

open AST
open Microsoft.FSharp.Quotations
open System.IO
open System.IO.Compression
open System.Collections.Concurrent

[<FunScript.JS>]
module Replacements =
    let utf8Encoding() = Core.Web.UTF8Encoding()

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

         ExpressionReplacer.createUnsafe 
            <@ fun () -> System.Text.Encoding.UTF8 @> 
            <@ fun () -> Replacements.utf8Encoding() @>

         ExpressionReplacer.createUnsafe 
            <@ fun (stream : Stream, length) -> stream.AsyncRead length @> 
            <@ fun (stream : Core.Web.Stream, length) -> stream.AsyncRead length @>

         ExpressionReplacer.createUnsafe 
            <@ fun (req : System.Net.WebRequest) -> req.AsyncGetResponse() @> 
            <@ fun (req : Core.Web.WebRequest) -> req.AsyncGetResponse() @>

         ExpressionReplacer.createUnsafe 
            <@ fun (stream : Stream, buffer, offset, count) -> stream.AsyncWrite(buffer, offset, count) @> 
            <@ fun (stream : Core.Web.Stream, buffer, offset, count) -> stream.AsyncWrite(buffer, offset, count) @> 

         ExpressionReplacer.createUnsafe 
            <@ fun () -> new MemoryStream() @> 
            <@ fun () -> Core.Web.Stream.Create() @>

         ExpressionReplacer.createUnsafe 
            <@ fun x (y : CompressionMode) z -> new GZipStream(x, y, z) @> 
            <@ fun x y z -> Core.Web.GZipStream.Create(x, y, z) @>
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

      ExpressionReplacer.createTypeMethodMappings
         typeof<System.IO.Stream>
         typeof<Core.Web.Stream>
         
      ExpressionReplacer.createTypeMethodMappings
         typeof<System.IO.MemoryStream>
         typeof<Core.Web.Stream>

      ExpressionReplacer.createTypeMethodMappings
         typeof<System.Net.WebHeaderCollection>
         typeof<Core.Web.WebHeaderCollection>

      ExpressionReplacer.createTypeMethodMappings
         typeof<System.Net.WebResponse>
         typeof<Core.Web.WebResponse>

      ExpressionReplacer.createTypeMethodMappings
         typeof<System.Net.WebRequest>
         typeof<Core.Web.WebRequest>

      // Note: this replaces all encodings with UTF8. However, we've only
      // mapped UTF8 at the moment. Therefore, this is ok.
      ExpressionReplacer.createTypeMethodMappings
         typeof<System.Text.Encoding>
         typeof<Core.Web.UTF8Encoding>

      ExpressionReplacer.createTypeMethodMappings
         typeof<System.Net.WebUtility>
         typeof<Core.Web.WebUtility>

   ] |> List.concat