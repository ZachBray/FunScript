// ---
// header: Movie Database
// tagline: Calling REST API using Apiary.io
// ---

[<ReflectedDefinition>]
module Program

open FunScript
open ApiaryProvider

// ------------------------------------------------------------------
// Initializataion

type MovieDb = ApiaryProvider<"themoviedb">

let jQuery(selector : string) = Globals.Dollar.Invoke selector
let (?) jq name = jq("#" + name)

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
  
    let! credits = movie.AsyncCredits()
    let sorted = credits.Cast |> Array.sortBy (fun c -> c.Order)    
    let sorted = 
      if sorted.Length <= 10 then sorted |> Seq.ofArray 
      else sorted |> Seq.ofArray |> Seq.take 10

    jQuery?dialogCast.html("") |> ignore
    for cast in credits.Cast do 
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
          jQuery("<a>").attr("data-toggle", "modal").
            attr("href", "#detailsDialog").text(item.Title).
            click(fun e -> showDetails item.Id |> Async.StartImmediate; null)

        let details = jQuery("<ul>")
        let date = 
          match item.ReleaseDate with
          | None -> "(not known)"
          | Some dt -> dt.ToString()
        jQuery("<li>").html("<strong>Released:</strong> " + date).
          appendTo(details) |> ignore
        jQuery("<li>").html("<strong>Average vote:</strong> " + item.VoteAverage.ToString()).
          appendTo(details) |> ignore
        jQuery("<li>").html("<strong>Popularity:</strong> " + item.Popularity.ToString()).
          appendTo(details) |> ignore

        let body = jQuery("<div>").addClass("searchResult")
        jQuery("<h3>").append([| box link |]).appendTo(body) |> ignore
        jQuery("<img>").attr("src", root + (defaultArg item.PosterPath "")).appendTo(body) |> ignore
        details.appendTo(body) |> ignore
        jQuery("<div>").addClass("clearer").appendTo(body) |> ignore
        body.appendTo(jQuery?results) |> ignore )}

  // ----------------------------------------------------------------
  // Movie search

  jQuery?searchButton.click(fun () ->
    let id = jQuery?searchInput._val() :?> string
    search id |> Async.StartImmediate )

// ------------------------------------------------------------------
let components =
  FunScript.Data.Components.DataProviders  
do Runtime.Run(components=components, directory="Web")