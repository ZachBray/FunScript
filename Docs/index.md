---
header: FunScript
tagline: F# to JavaScript with type providers
---

    [hide]
    open FSharp.Data
    open FunScript
    open FunScript.TypeScript
    
    type System.Array with
      member x.push(element:obj) = ()

<div class="row"><div class="large-7 columns" id="hp-snippet">

    // Access standard JavaScript libraries in a type-safe way
    type j = Api<"../../Examples/Typings/jquery.d.ts">
    type h = Api<"../../Examples/Typings/highcharts.d.ts">

    // Integrate REST APIs with F# 3.0 type providers
    type WorldBank = WorldBankDataProvider<Asynchronous=true>
    let data = WorldBank.GetDataContext()

    // Get full type checking for external data sources!
    let countries = 
      [ data.Countries.Denmark
        data.Countries.``Czech Republic``
        data.Countries.``United Kingdom``
        data.Countries.``United States`` ]

    // Write asynchronous computations without callbacks
    let render () = async {
      let chart = h.HighchartsOptions((*[omit:(...)]*)chart = h.HighchartsChartOptions(renderTo = "chart", ``type`` = "line"), title = h.HighchartsTitleOptions(text = "University enrollment"), series = [| |] (*[/omit]*))
      for country in countries do

        // Access data sets in a statically typed way
        let data = country.Indicators
        let! l = data.``School enrollment, tertiary (% gross)``

        // Add line series to the chart
        h.HighchartsSeriesOptions(data=l, name=country.Name)
        |> chart.series.push }


  <div class="row"><div class="large-6 columns">

### Contributions

FunScript is open-source and has been created by F# experts and active community contributors.

 - Zach Bray ([@zbray](https://twitter.com/zbray))
 - Tomas Petricek ([@tomaspetricek](https://twitter.com/tomaspetricek))
 - Robert Pickering ([@robertpi](http://twitter.com/robertpi))
 - James Freiwirth ([@endeavour](https://github.com/endeavour))

Help us make [improve FunScript](contribute.html)!

  </div><div class="large-6 columns">

### Samples and tutorials
For more information, explore [examples on GitHub](https://github.com/ZachBray/FunScript/tree/master/Examples)
and take a look at our tutorials and samples:

 - [Getting started tutorial](samples/tutorial.html)
 - [Using HTML5 canvas](samples/canvas.html)
 - [Asynchornous computations](samples/simpleasync.html)
 - [Visualizing World Bank data](samples/worldbank.html)
 - [Calling The Movie Database](samples/moviedatabase.html)
 - [Brainfuck interpreter!!](samples/brainfuck.html)

</div></div>

</div><div class="large-5 columns">

**FunScript** is a lightweight F# library that lets you rapidly develop single-page 
applications. You can connect to external data sources and call REST APIs in a type 
safe way, produce dashboards using JavaScript visualization libraries and write 
asynchronous computations easily without explicit callbacks.

<div style="margin-left:auto;margin-right:auto;text-align:center;">
<a href="get.html" class="success button">Get FunScript today!</a>
</div>

## What do you get

 * Write **client-side code in F#** with the full comfort of MonoDevelop, Visual Studio or Emacs 
 * Get **autocomplete and tooltips** for the HTML DOM, jQuery and other popular JavaScript libraries
 * Use language with **sound generics** and support for **asynchronous** programming 
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