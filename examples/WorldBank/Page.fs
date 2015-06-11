// ---
// header: WorldBank
// tagline: Getting and visualizing data
// ---

(*** hide ***)
[<ReflectedDefinition>]
module Program

open FunScript
open FunScript.TypeScript
open FSharp.Data

(**

This sample builds a dashboard for comparing university enrollment using F# type provider for the 
WorldBank data, FunScript F# to JavaScript compiler with a type provider for TypeScript definitions
and Highcharts charting library for JavaScript.

The code is based on Tomas Petricek's sample from TechMesh 2012 called [How F# Learned to Stop Worrying and Love the Data](http://techmeshconf.com/techmesh-london-2012/presentation/How%20F#%20Learned%20to%20Stop%20Worrying%20and%20Love%20the%20Data).

## Type providers and jQuery

To get the data, we're using the WorldBank type provider from the [F# Data library](http://fsharp.github.io/FSharp.Data/).
FunScript supports the type provider and automatically translates the code to
corresponding JavaScript implementation. We want to access the default 
"World Development Indicators" database that contains the information, but we 
need to configure the provider to call WorldBank asynchronously: 
*)

type WorldBank = WorldBankDataProvider<Asynchronous=true>

// Allows writing jq?name for element access
let jq(selector : string) = Globals.Dollar.Invoke selector
let (?) jq name = jq("#" + name)

(**
In addition to referencing the WorldBank, the snippet also loaded jQuery 
and Highcharts, which is the charting library that we use in this sample. Now we need
to connect to the WorldBank and build a list of countries that we want to offer.
Here, we just pick a couple of interesting countries by hand:
*)

let data = WorldBank.GetDataContext()

let countries () = 
 [| data.Countries.``Arab World`` 
    data.Countries.``European Union``
    data.Countries.Australia
    data.Countries.Brazil
    data.Countries.Canada
    (*[omit:(more countries)]*)
    data.Countries.Chile
    data.Countries.China
    data.Countries.``Czech Republic``
    data.Countries.Denmark
    data.Countries.France
    data.Countries.Greece
    data.Countries.``Low income``
    data.Countries.``High income``
    data.Countries.``United Kingdom``
    data.Countries.``United States`` (*[/omit]*) |]

(**
## Building user interface

As a first step, we need to iterate over all the countries and generate check box
for for each country. This can be done using the `jq` function defined earlier
(which builds the HTML element) and then using standard jQuery methods like 
`attr` and `append` which are exposed automatically using TypeScript type provider:

*)

// Create check boxes for all countries
let makeCheckInfos() = 
  countries () |> Array.mapi (fun index country ->
    // Create the checkbox element
    let input = jq("<input>")
    input.attr("type", "checkbox") |> ignore
    // Create label with checkbox & text
    let label = jq("<label>")
    label.append([| box input |]) |> ignore
    label.append([| box country.Name |]) |> ignore
    // Append elements to one of 3 columns
    let panel = (index % 3) + 1
    let panelId = "#countryList" + panel.ToString()
    label.appendTo(jq(panelId)) |> ignore
    // Return the country and checkbox element
    country, input )

(**

The function returns a collection of pairs consisting of the country
and the checkbox element. This means that we can now easily iterate over 
the resulting collection, test which checkboxes are clicked and get the
data for the corresponding countries.

## Generating charts and user interaction

The rest of the code is short enough that we can show it in a single snippet.
The `main` function contains a nested function `render` followed by a short
block that does initialization.

In the `render` function, we create Highcharts chart object, iterate over all
countries, test which country should be included (using standard jQuery `is(":checked")` call
and then we get the data and render it.

Note that the `render` function is asynchronous (wrapped in the `async { .. }` block).
This means that we can load data using `let!` without blocking the browser. In JavaScript,
this is automatically translated to a callback.
*)

let main() = 
  let infos = makeCheckInfos()

  // Render the chart based on checkboxes
  let render () = async {
    // Create a line chart
    let opts = createEmpty<HighchartsOptions>()
    let titleOptions = createEmpty<HighchartsTitleOptions>()
    titleOptions.text <- "School enrollment, tertiary (% gross)"
    opts.title <- titleOptions
    let subTitleOptions = createEmpty<HighchartsSubtitleOptions>()
    subTitleOptions.text <- "Source: WorldBank"
    opts.subtitle <- subTitleOptions
    opts.series <- [| |]
    
    
    // Create series we want to render
    for country, check in infos do
      if check._is ":checked" then
        // Asynchronously load data without blocking
        let! vals = country.Indicators.``School enrollment, tertiary (% gross)``
        // Convert data to format required by HighCharts
        let data = 
            vals
            |> Seq.map (fun (k, v) ->
                let p = createEmpty<HighchartsDataPoint>() 
                p.x <- number k
                p.y <- number v
                p)
            |> Seq.toArray
        // Create new data series and add it to the chart
        let series = createEmpty<HighchartsSeriesOptions>()
        series.data <- unbox data
        series.name <- country.Name
        series._type <- "line"
        
        opts.series.pushOverload2(series) |> ignore
    // Invoke constructor on a chart prototype
    let chartElement = jq?chart
    chartElement.highcharts(opts) |> ignore
  }

  // Register click handlers
  render () |> Async.StartImmediate
  infos |> Array.iter (fun (_, check) ->
    // When checkbox is clicked, start the 'render' workflow
    check.click(fun _ -> 
      render() |> Async.StartImmediate |> box) |> ignore)

(**
As already mentioned, the `render` function is asynchronous. Because we configured
the WorldBank type provider to expose only asynchronous API (by setting `Asynchronous=true`)
the type of the property `School enrollment, tertiary (% gross)` is an asynchronous
workflow `Async<Indicator>`. This means that it represents a computation that can be
invoked using a callback. In F#, we do not have to write callbacks explicitly - we can
just call the workflow using the `let!` keyword and the code is automatically transformed.
This also allows us to use `for` loop rather than building a complex chain of callbacks.

Finally, when we want to invoke `render()` we can use standard `Async.StartImmediate` 
operation that starts the work. This runs the first part of the function until the first
`let!` point and then runs the rest of the workflow in a callback (while the caller is
free to finish whatever it was doing - like registering event handlers in our `main` function).

## Configuring FunScript runner

To use the WorldBank type provider in FunScript, we need to tell FunScript to use the
translation components that handle F# Data type proivders. Then we can use the sample
runner as usual:
*)

let components = 
  FunScript.Data.Components.getDataProviders()
do Runtime.Run(components=components, directory="Web")