// ---
// header: Movie Database
// tagline: Calling REST API using Apiary.io
// ---

[<ReflectedDefinition>]
module Program

open FunScript
open FunScript.TypeScript
open ApiaryProvider

// ------------------------------------------------------------------
// Initializataion

type MovieDb = ApiaryProvider<"themoviedb">

// Allows writing jq?name for element access
let jq(selector : string) = Globals.Dollar.Invoke selector
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
    jq?dialogOverview.text(movie.Overview) |> ignore 
    jq?dialogTitle.text(movie.Title) |> ignore
    jq?dialogImage.attr("src", root + movie.PosterPath) |> ignore
  
    let! credits = movie.AsyncCredits()
    let sorted = credits.Cast |> Array.sortBy (fun c -> c.Order)    
    let sorted = 
      if sorted.Length <= 10 then sorted |> Seq.ofArray 
      else sorted |> Seq.ofArray |> Seq.take 10

    jq?dialogCast.html("") |> ignore
    for cast in credits.Cast do 
      let html = "<strong>" + cast.Name + "</strong> (" + cast.Character + ")"
      let li = jq("<li>")
      li.html(html) |> ignore
      li.appendTo(jq?dialogCast) |> ignore }

  // ----------------------------------------------------------------
  // Movie search

  let search term = async {
    let! res = db.Search.AsyncMovie(query=["query", term])
    jq?results.html("") |> ignore

    res.Results 
    |> Seq.ofArray
    |> Seq.iteri (fun index item ->
        
        let link = 
          jq("<a>").attr("data-toggle", "modal").
            attr("href", "#detailsDialog").text(item.Title).
            click(fun e -> showDetails item.Id |> Async.StartImmediate; null)

        let details = jq("<ul>")
        let date = 
          match item.ReleaseDate with
          | None -> "(not known)"
          | Some dt -> dt.ToString()
        jq("<li>").html("<strong>Released:</strong> " + date).
          appendTo(details) |> ignore
        jq("<li>").html("<strong>Average vote:</strong> " + item.VoteAverage.ToString()).
          appendTo(details) |> ignore
        jq("<li>").html("<strong>Popularity:</strong> " + item.Popularity.ToString()).
          appendTo(details) |> ignore

        let body = jq("<div>").addClass("searchResult")
        jq("<h3>").append([| box link |]).appendTo(body) |> ignore
        jq("<img>").attr("src", root + (defaultArg item.PosterPath "")).appendTo(body) |> ignore
        details.appendTo(body) |> ignore
        jq("<div>").addClass("clearer").appendTo(body) |> ignore
        body.appendTo(jq?results) |> ignore )}

  // ----------------------------------------------------------------
  // Movie search

  jq?searchButton.click(fun _ ->
    let id = jq?searchInput._val() :?> string
    search id |> Async.StartImmediate |> box)

// ------------------------------------------------------------------
let components =
  FunScript.Data.Components.getDataProviders()  
do Runtime.Run(components=components, directory="Web")