//---
//header: FunScript
//tagline: F# to JavaScript with type providers
//---

(**
<div class="row"><div class="large-7 columns" id="hp-snippet">
*)

// Allow access to the F# AST
[<ReflectedDefinition>]
module Sample

open FSharp.Data
open FunScript

// Access standard JavaScript libraries in a type-safe way.
// Generate strongly-typed interfaces from TypeScript 0.9.x 
// definitions files or use any of the 280+ pre-built library
// mappings (hosted on NuGet).
#I "../Examples/Typings/"
#r "FunScript.TypeScript.Binding.lib.dll"
#r "FunScript.TypeScript.Binding.jquery.dll"
#r "FunScript.TypeScript.Binding.highcharts.dll"

// Integrate REST APIs with F# 3.0 type providers
type WorldBank = WorldBankDataProvider<Asynchronous=true>
let data = WorldBank.GetDataContext()

// Get full type checking for external data sources!
let countries = 
  [ data.Countries.Denmark
    data.Countries.``Czech Republic``
    data.Countries.``United Kingdom``
    data.Countries.``United States`` ]

// Easily define strongly-typed Foreign Function Interfaces
// (FFIs) to access unmapped functions
[<JSEmitInlineAttribute("({0} * 1.0)")>]
let number(x : int) : float = failwith "never"

// Write asynchronous computations without callbacks
let render () = async {
  let chart = createEmpty<HighchartsOptions>()
  chart.series <- [| |]
  for country in countries do

    // Access data sets in a statically typed way
    let data = country.Indicators
    let! l = data.``School enrollment, tertiary (% gross)``

    // Add line series to the chart
    let seriesOpts = createEmpty<HighchartsSeriesOptions>()
    seriesOpts.name <- country.Name
    seriesOpts.data <- 
        [| for t, y in l -> [| number t; y |] :> obj |]

    // Use a standard library function 
    // through ...Binding.lib.dll
    chart.series.push seriesOpts |> ignore 
}

(*** hide ***)
#r "../ThirdParty/FSharp.Data.dll"
#r "../FunScript/bin/Debug/FunScript.dll"
#r "../FunScript.Interop/bin/Debug/FunScript.Interop.dll"

(**

  <div class="row"><div class="large-6 columns">

### Contributions

FunScript is open-source and created by F# experts and active community contributors.

 - Zach Bray ([@zbray](https://twitter.com/zbray))
 - Tomas Petricek ([@tomaspetricek](https://twitter.com/tomaspetricek))
 - Robert Pickering ([@robertpi](http://twitter.com/robertpi))
 - James Freiwirth ([@endeavour](https://github.com/endeavour))

Help us [improve FunScript](contribute.html)!

  </div><div class="large-6 columns">

### Samples and tutorials
For more information, explore [examples on GitHub](https://github.com/ZachBray/FunScript/tree/master/Examples)
and take a look at our tutorials and samples:

 - [Getting started tutorial](samples/tutorial/index.html)
 - [Mario tutorial](samples/mario/index.html) & [PacMan game](samples/pacman/index.html)
 - [Visualizing World Bank data](samples/worldbank/index.html)
 - [Calling The Movie Database](samples/moviedatabase/index.html)
 - [Mandelbrot with HTML5 canvas](samples/mandelbrot/index.html)
 - [Asynchronous computations](samples/simpleasync/index.html)

</div></div>

</div><div class="large-5 columns">

**FunScript** is a lightweight F# library that lets you rapidly develop single-page 
applications. You can connect to external data sources and call REST APIs with intellisense, 
produce dashboards using JavaScript visualization libraries and write 
asynchronous computations easily without explicit callbacks.

<div style="margin-left:auto;margin-right:auto;text-align:center;">
<a href="get.html" class="success button">Get FunScript today!</a>
</div>

## What do you get

 * Write **client-side code in F#** with the full comfort of MonoDevelop, Visual Studio, [TsunamiIDE](http://tsunami.io/) Vim or Emacs 
 * Get **autocomplete and tooltips** for the HTML DOM, jQuery and other popular JavaScript libraries
 * Use language with **generics**, **reflection** and support for **asynchronous** programming 
 * Process JSON and call REST APIs using F# 3.0 type providers with no hassle!

## FunScript on Channel 9

Tomas talks about type providers, the [F# Data library](http://fsharp.github.io/FSharp.Data) and 
FunScript on Channel 9 and shows how you can easily visualize [World Bank data](http://data.worldbank.org/)
and connect to [The Movie Database](http://www.themoviedb.org/).

<div style="margin-left:auto;margin-right:auto;text-align:center;">
<a href="http://channel9.msdn.com/posts/Tomas-Petricek-How-F-Learned-to-Stop-Worrying-and-Love-the-Data">
<img src="img/talk.jpg" alt="Tomas Petricek: How F# Learned to Stop Worrying and Love the Data" style="border:none"/>
</a>
</div>

</div>
</div>
*)
