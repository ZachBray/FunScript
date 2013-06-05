// --------------------------------------------------------------------------------------
// Mappings for the WorldBank Type Provider runtime
// --------------------------------------------------------------------------------------

module private FunScript.Data.WorldBank

open System
open FunScript
open FunScript.AST
open FunScript.Data.Utils
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection

type WorldBankResponse = FSharp.Data.JsonProvider<"worldbank.json">

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
              else Some(int v.Date, float v.Value.Number.Value))
          cont res ))

open Runtime
open ProviderImplementation
open FSharp.Data.RuntimeImplementation.WorldBank

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
  [ ExpressionReplacer.createUnsafe <@ fun (w:IWorldBankData) -> w.GetCountries @> <@ WorldBankRuntime.GetCountries @> 
    ExpressionReplacer.createUnsafe <@ fun (w:ICountry) -> w.GetIndicators @> <@ WorldBankRuntime.GetIndicators @> 
    ExpressionReplacer.createUnsafe <@ fun (w:ICountryCollection) -> w.GetCountry @> <@ WorldBankRuntime.GetCountry @> 
    ExpressionReplacer.createUnsafe <@ fun (w:IIndicators) -> w.AsyncGetIndicator @> <@ WorldBankRuntime.AsyncGetIndicator @> 
    worldbank
  ]