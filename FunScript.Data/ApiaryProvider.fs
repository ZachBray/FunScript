// --------------------------------------------------------------------------------------
// Mappings for the Apiary Type Provider runtime
// --------------------------------------------------------------------------------------

module private FunScript.Data.ApiaryProvider

open System
open System.Reflection
open FSharp.Data.Json
open FSharp.Data.RuntimeImplementation
open FSharp.Data.RuntimeImplementation.Apiary
open FunScript
open FunScript.AST
open FunScript.Data.Utils
open ProviderImplementation
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection

// --------------------------------------------------------------------------------------
// Reimplemntation of the Apiary provider runtime

[<JS>]
module Runtime = 
  type ApiaryJsContext = 
    { Root : string
      mutable GlobalQuery : (string * string)[] 
      mutable GlobalHeaders : (string * string)[]
      mutable GlobalArguments : (string * string)[] }

  [<JS; JSEmit("{0}.get_Context = function() { return {1}; };")>]
  let setContext(o:obj, ctx:obj) : unit = ()

  [<JS; JSEmit("return {0}.get_Context();")>]
  let getContext(o:obj) : InternalApiaryContext = failwith "never"

  [<JS>]
  type ApiaryJsRuntime =

    static member AsyncMap(work, f) = async {
      let! v = work in return f v }

    static member ParseHeaders (headers:string) = 
      headers.Split('\n') |> Array.choose (fun h ->
          if String.IsNullOrEmpty(h) then None else
            let arr = h.Split(':') 
            if arr.Length = 2 then Some(arr.[0], arr.[1])
            else failwith "Wrong headers" ) // : '%s'" headers )
    
    static member ProcessParameters(reqHeaders, headers, query) =
      let headers = Array.append (ApiaryJsRuntime.ParseHeaders(reqHeaders)) (Array.ofList (emptyIfNull headers))
      let query = emptyIfNull query
      headers, Array.ofList query

    static member AddQueryParam(x : ApiaryJsContext, key:string, value:string) =
      x.GlobalQuery <- Array.append x.GlobalQuery [| key, value |]
    
    static member AddHeader(x : ApiaryJsContext, key:string, value:string) =
      x.GlobalHeaders <- Array.append x.GlobalHeaders [| key, value |]

    static member AsyncInvokeOperation
        ( x : ApiaryJsContext, 
          { Method = meth; Path = path; Arguments = args;
            Headers = headers; Query = query }) = 
      Async.FromContinuations(fun (cont, econt, ccont) -> 

        // Replace parameters in the path with actual arguments
        let allArguments = Array.append x.GlobalArguments args
        let path = 
          allArguments |> Seq.fold (fun (path:string) (key, value) -> 
            path.Replace(key, value)) path

        // Run the HTTP request
        let allheaders = Array.append headers x.GlobalHeaders
        let allquery = Array.append query x.GlobalQuery
        if meth.ToLower() = "get" then
        
          // Construct url & query
          let url = x.Root + path
          let queryString = 
            allquery 
            |> Array.map (fun (k, v) -> k + "=" + encodeURIComponent(v))
            |> String.concat "&"
        
          let xhr = newXMLHttpRequest()
          xhr.``open``("GET", url + "?" + queryString)
          allheaders |> Array.iter (fun (header, value) ->
            xhr.setRequestHeader(header, value) )

          xhr.onreadystatechange <- (fun () ->
            if xhr.readyState = 4.0 then
              let source = xhr.responseText
              let doc = JsonDocument.Create(JsonValue.Parse(source))
              setContext(doc, { x with GlobalArguments = allArguments } )
              cont doc
            )
          xhr.send("") 
        else 
          failwith "Only GET supported" )


// --------------------------------------------------------------------------------------
// Define the mappings

open Runtime

// Deserialize the quotation for performance reasons
let private newApiaryCtx = <@@ new ApiaryContext(undef()) @@>
let private newApiaryDoc = <@@ new ApiaryDocument(undef()) @@>

let components = 
  [ // ApiaryDocument behaves just like JsonDocument
    ExpressionReplacer.createUnsafe <@ fun (d:ApiaryDocument) -> d.JsonValue @> <@ JsonProvider.JsRuntime.Identity @>
    ExpressionReplacer.createUnsafe <@ fun (d:ApiaryDocument) -> d.Context @> <@ getContext @>

    // Apiary runtime is reimplemented by ApiaryJsRuntime
    ExpressionReplacer.createUnsafe <@ ApiaryRuntime.ProcessParameters @> <@ ApiaryJsRuntime.ProcessParameters @> 
    ExpressionReplacer.createUnsafe <@ fun (a:ApiaryContext) -> a.AddHeader @> <@ ApiaryJsRuntime.AddHeader @> 
    ExpressionReplacer.createUnsafe <@ fun (a:ApiaryContext) -> a.AddQueryParam @> <@ ApiaryJsRuntime.AddQueryParam @> 
    ExpressionReplacer.createUnsafe <@ fun (a:ApiaryOperations) -> a.AsyncInvokeOperation @> <@ ApiaryJsRuntime.AsyncInvokeOperation @> 
    ExpressionReplacer.createUnsafe <@ ApiaryGenerationHelper.AsyncMap @> <@ ApiaryJsRuntime.AsyncMap @> 

    // Turn 'new ApiaryDocument(...)' to just 'return {0}'
    CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | SpecificConstructor newApiaryDoc (_, [rootArgument]) ->
         let (Split(valDecls, valRef)) = rootArgument
         [ yield! valDecls
           yield returnStategy.Return <| valRef ]
      | _ -> []

    // Construction of ApiaryContext becomes just a record
    CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | SpecificConstructor newApiaryCtx (_, [rootArgument]) ->
         let (Split(valDecls, valRef)) = rootArgument 
         let fields = 
            [ "Root", valRef
              "GlobalQuery", Array []
              "GlobalHeaders", Array []
              "GlobalArguments", Array [] ]
         [ yield! valDecls
           yield returnStategy.Return <| Object fields ]
      | _ -> []
  ]