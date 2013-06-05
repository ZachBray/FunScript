[<ReflectedDefinition>]
module Program

open FunScript
open FSharp.Data

// ------------------------------------------------------------------
// Initializataion

type MovieDb = ApiaryProvider<"themoviedb">

type j = TypeScript.Api<"../Typings/jquery.d.ts">
let jQuery (command:string) = j.jQuery.Invoke(command)
let (?) jq name = jQuery("#" + name)

type System.Object with
  member x.asJQuery() : j.JQuery = unbox x

// ------------------------------------------------------------------
// Main function

let main() = 

  let db = new MovieDb("http://api.themoviedb.org")
  db.AddQueryParam("api_key", "6ce0ef5b176501f8c07c634dfa933cff")
  let root = "http://cf2.imgobject.com/t/p/w92/"

  // ----------------------------------------------------------------
  // Show details

  let showDetails (id:int) = async {
    let! movie = db.Movie.AsyncGetMovie(id.ToString())
    jQuery?dialogOverview.text(movie.Overview) |> ignore 
    jQuery?dialogTitle.text(movie.Title) |> ignore
    jQuery?dialogImage.attr("src", root + movie.PosterPath) |> ignore
  
    let! casts = movie.AsyncCasts()
    let sorted = casts.Cast |> Array.sortBy (fun c -> c.Order)    
    let sorted = 
      if sorted.Length <= 10 then sorted |> Seq.ofArray 
      else sorted |> Seq.ofArray |> Seq.take 10

    jQuery?dialogCast.html("") |> ignore
    for cast in casts.Cast do 
      let html = "<strong>" + cast.Name + "</strong> (" + cast.Character + ")"
      let li = jQuery("<li>")
      li.html(html) |> ignore
      li.appendTo(jQuery?dialogCast) |> ignore }

  // ----------------------------------------------------------------
  // Movie search

  let search term = async {
    let! res = db.Search.AsyncMovie(query=["query", term])
    jQuery?results.html("") |> ignore

    res.Results 
    |> Seq.ofArray
    |> Seq.iteri (fun index item ->
        let link = 
          jQuery("<a>").attr("data-toggle", "modal").asJQuery().
            attr("href", "#detailsDialog").asJQuery().text(item.Title).asJQuery().
            click(fun _ -> showDetails item.Id |> Async.StartImmediate ).asJQuery()

        let details = jQuery("<ul>")
        let date = 
          match item.ReleaseDate.DateTime with
          | None -> "(not known)"
          | Some dt -> dt.ToString()
        jQuery("<li>").html("<strong>Released:</strong> " + date).
          asJQuery().appendTo(details) |> ignore
        jQuery("<li>").html("<strong>Average vote:</strong> " + item.VoteAverage.ToString()).
          asJQuery().appendTo(details) |> ignore
        jQuery("<li>").html("<strong>Popularity:</strong> " + item.Popularity.ToString()).
          asJQuery().appendTo(details) |> ignore

        let body = jQuery("<div>").addClass("searchResult").asJQuery()
        jQuery("<h3>").append([| box link |]).asJQuery().appendTo(body) |> ignore
        jQuery("<img>").attr("src", root + item.PosterPath).asJQuery().appendTo(body) |> ignore
        details.appendTo(body) |> ignore
        jQuery("<div>").addClass("clearer").asJQuery().appendTo(body) |> ignore
        body.appendTo(jQuery?results) |> ignore )}

  // ----------------------------------------------------------------
  // Movie search

  jQuery?searchButton.click(fun () ->
    let id = jQuery?searchInput.``val``() :?> string
    search id |> Async.StartImmediate )

// ------------------------------------------------------------------
let components = 
  FunScript.Interop.Components.all @
  FunScript.Data.Components.DataProviders  
do Runtime.Run(components=components, directory="Web")