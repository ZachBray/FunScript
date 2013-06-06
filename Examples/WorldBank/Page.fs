// ---
// header: WorldBank
// tagline: Getting and visualizing data
// ---

[<ReflectedDefinition>]
module Program

open FunScript
open FSharp.Data

// ------------------------------------------------------------------
// Initializataion

type WorldBank = WorldBankDataProvider<"World Development Indicators", Asynchronous=true>

type j = TypeScript.Api<"../Typings/jquery.d.ts">
type h = TypeScript.Api<"../Typings/highcharts.d.ts">

let jQuery (command:string) = j.jQuery.Invoke(command)

// ------------------------------------------------------------------
// World bank countries

let data = WorldBank.GetDataContext()

let countries () = 
 [| data.Countries.``Arab World`` 
    data.Countries.``European Union``
    data.Countries.Australia
    data.Countries.Brazil
    data.Countries.Canada
    data.Countries.Chile
    data.Countries.``Czech Republic``
    data.Countries.Denmark
    data.Countries.France
    data.Countries.Greece
    data.Countries.``Low income``
    data.Countries.``High income``
    data.Countries.``United Kingdom``
    data.Countries.``United States`` |]

// ------------------------------------------------------------------
// Main function

let main() = 

  // Create check boxes for all countries
  let infos = countries () |> Array.toList |> List.mapi (fun index country ->
    let input = jQuery("<input>")
    input.attr("type", "checkbox") |> ignore
    let label = jQuery("<label>")
    label.append([| box input |]) |> ignore
    label.append([| box country.Name |]) |> ignore
    let panel = floor (index % 3) + 1
    label.appendTo(jQuery("#countryList" + panel.ToString())) |> ignore
    country, input )

  // Render the chart based on checkboxes
  let render () = async {
    let opts = h.HighchartsOptions()
    opts.chart <- h.HighchartsChartOptions(renderTo = "chart", ``type`` = "line")
    opts.title <- h.HighchartsTitleOptions(text = "School enrollment, tertiary (% gross)")
    opts.series <- [| |]
    
    // Create series we want to render
    let series = ref []
    for country, check in infos do
      if check.is(":checked") |> unbox then
        let! vals = country.Indicators.``School enrollment, tertiary (% gross)``
        let data = vals |> Seq.map (fun (k, v) -> [| number k; number v |]) |> Array.ofSeq
        opts.series.push(h.HighchartsSeriesOptions(data=data, name=country.Name))
    clone(h.Highcharts.Chart, opts) |> ignore }
  
  // Register click handlers
  render () |> Async.StartImmediate
  for _, check in infos do
    check.click(fun _ -> 
      render() |> Async.StartImmediate |> box) |> ignore

// ------------------------------------------------------------------
let components = 
  FunScript.Data.Components.DataProviders @ 
  FunScript.Interop.Components.all
do Runtime.Run(components=components, directory="Web")