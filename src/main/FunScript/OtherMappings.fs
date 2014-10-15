module internal FunScript.OtherMappings

open AST
open Microsoft.FSharp.Quotations
open System.IO
open System.IO.Compression
open System.Collections.Concurrent

[<FunScript.JS>]
module Replacements =
    let utf8Encoding() = Core.Web.UTF8Encoding()

[<FunScript.JS>]
module Extensions =
    [<FunScript.JSEmitInline("console.log({0})")>]
    let log(o: obj): unit = failwith "never"

    let logFormat(str: string, [<System.ParamArray>] args: obj[]) =
        if args.Length > 0 then log( System.String.Format(str, args) )
        else log(str)

    [<FunScript.JSEmit("if ({0}) { console.log({1}) }")>]
    let logIf(condition: bool, o: obj): unit = failwith "never"

// NOTE: A call replacer with System.Diagnostics.Debug will fail if FunScript is compiled in Release mode, so use this function instead
let private debugComponents =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.Call(None,mi,args) when mi.DeclaringType.Name = "Debug" ->
        let compile = fun quote -> let mi, _ = Quote.toMethodInfoFromLambdas quote
                                   compiler.Compile returnStrategy (ExpressionReplacer.buildCall mi args)
        match mi.Name with
        | "WriteLine" -> compile <@ Extensions.logFormat @>
        | "WriteLineIf" -> compile <@ Extensions.logIf @>
        | _ -> []
      | _ -> []


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

         ExpressionReplacer.createUnsafe
            <@ fun (str: string, args: obj[]) -> System.Console.WriteLine(format=str, arg=args) @>
            <@ Extensions.logFormat @>

         ExpressionReplacer.createUnsafe 
            <@ fun () -> Event<_>() @> 
            <@ fun () -> Core.Events.Event.Create() @>

         ExpressionReplacer.createUnsafe 
            <@ fun (evt : Event<_>, arg) -> evt.Trigger arg @> 
            <@ fun (evt : Core.Events.Event<_>, arg) -> evt.Trigger arg @>

         ExpressionReplacer.createUnsafe 
            <@ fun (evt : Event<_>) -> evt.Publish @> 
            <@ fun (evt : Core.Events.Event<_>) -> evt.Publish @>

         debugComponents
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

      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Control.ObservableModule"
         "FunScript" "FunScript.Core.Events.Observable"
   ] |> List.concat