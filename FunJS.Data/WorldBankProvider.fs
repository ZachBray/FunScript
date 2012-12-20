module private FunJS.Data.WorldBank

open System
open FunJS
open FunJS.AST
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection

type WorldBankResponse = FSharp.Data.JsonProvider<"worldbank.json">

let private undef() = failwith "!"

let (|SpecificConstructor|_|) templateParameter = 
  match templateParameter with
  | Patterns.NewObject(cinfo1, _) -> (function
      | Patterns.NewObject(cinfo2, args) 
            when cinfo1.MetadataToken = cinfo2.MetadataToken ->
          Some(cinfo1, args)
      | _ -> None)
  | _ -> invalidArg "templateParameter" "Unrecognized quotation: Must be NewObject."

[<JS; JSEmit("return {0}==null?List_Empty():{0};")>]
let emptyIfNull (list:_ list) = list

[<JS; JSEmit("return $.getJSON({0}, {1});")>]
let getJSON (url:string, callback : string -> unit) : unit = failwith "never"

[<JS; JSEmit("return encodeURIComponent({0});")>]
let encodeURIComponent(s:string) : string = failwith "never"

[<JS>]
module Runtime = 
  type WorldBankData = 
    { ServiceUrl : string
      Source : string } 

  type WorldBankCountries = WorldBankData

  type WorldBankCountry =
    { Context : WorldBankData
      Code : string 
      Name : string }

  type WorldBankIndicators = WorldBankCountry
          
  let worldBankUrl (wb:WorldBankData) (functions: string list) (props: (string * string) list) = 
    wb.ServiceUrl + "/" +
    (functions |> List.map (fun m -> "/" + encodeURIComponent(m)) |> String.concat "") +
    "?per_page=1000" +
    (props |> List.map (fun (key, value) -> "&" + key + "=" + encodeURIComponent(value:string)) |> String.concat "")

  [<JS>]
  type WorldBankRuntime =
    static member GetCountries(data:WorldBankData) : WorldBankCountries=
      data
    static member GetCountry(countries:WorldBankCountries, code:string, name:string) : WorldBankCountry =
      { Context = countries
        Code = code 
        Name = name }
    static member GetIndicators(country:WorldBankCountry) : WorldBankIndicators =
      country

    static member AsyncGetIndicator(country:WorldBankIndicators, indicator:string) : Async<seq<int * float>> =
      Async.FromContinuations(fun (cont, econt, ccont) ->
        let wb = country.Context
        let countryCode = country.Code
        let url = worldBankUrl wb [ "countries"; countryCode; "indicators"; indicator ] [ "date", "1900:2050"; "format", "jsonp" ]
        getJSON(url + "&prefix=?", fun json ->
          let data = WorldBankResponse.Parse(json)
          let res =
            data.Array |> Seq.ofArray |> Seq.choose (fun v ->
              if JsonProvider.JsHelpers.isNull v.Value then None
              else Some(int v.Date, float v.Value))
          cont res ))

open Runtime
open ProviderImplementation

let private newWorldBankData = <@@ new WorldBankData(undef(), undef()) @@>
let private worldbank =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.PropertyGet (Some inst, pi, []) when pi.Name = "Name" && pi.DeclaringType = typeof<Country> ->
         let (Split(objDecls, objRef)) = inst
         [ yield! objDecls
           yield returnStategy.Return <| PropertyGet(objRef, "Name") ]

      | SpecificConstructor newWorldBankData (_, [serviceUrl; source]) ->
         let (Split(urlDecls, urlRef)) = serviceUrl
         let (Split(sourceDecls, sourceRef)) = source
         let fields = 
            [ "ServiceUrl", urlRef
              "Source", sourceRef ]
         [ yield! sourceDecls
           yield! urlDecls
           yield returnStategy.Return <| Object fields
         ]
      | _ -> []

let components = 
  [ ExpressionReplacer.createUnsafe <@ fun (w:WorldBankData) -> w._GetCountries @> <@ WorldBankRuntime.GetCountries @> 
    ExpressionReplacer.createUnsafe <@ fun (w:Country) -> w._GetIndicators @> <@ WorldBankRuntime.GetIndicators @> 
    ExpressionReplacer.createUnsafe <@ fun (w:CountryCollection<Country>) -> w._GetCountry @> <@ WorldBankRuntime.GetCountry @> 
    ExpressionReplacer.createUnsafe <@ fun (w:Indicators) -> w._AsyncGetIndicator @> <@ WorldBankRuntime.AsyncGetIndicator @> 
    worldbank
  ]