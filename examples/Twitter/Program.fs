[<ReflectedDefinition>]
module Program

open FunScript
open FunScript.TypeScript

type Tweet =
 { from_user: string
   text: string }

type TwitterSearch =
   { results: Tweet[] }

type Program() =

   // Allows writing jq?name for element access
   let jq(selector : string) = Globals.Dollar.Invoke selector
   let (?) jq name = jq("#" + name)

   let map = 
      let mapElement = jq?map
      google.maps.Map.Create(mapElement.[0])

   let mutable markers = List.empty

   let showTweets (search:TwitterSearch) =
      let tweetList =
         search.results |> Array.map (fun tweet ->
            "<p><b>" + tweet.from_user + "</b>&nbsp;" + tweet.text + "</p>")
         |> Array.map box
      jq?tweets.children().remove() |> ignore
      jq?tweets.append tweetList |> ignore
   
   let clearMap() =
      markers |> List.iter (fun (marker:google.maps.Marker) ->
         marker.setMap(unbox<google.maps.Map> null))
      markers <- List.empty

   let query searchText (callback:'a -> unit) =
      let settings = createEmpty<JQueryAjaxSettings>()
      settings.``success <-``(fun data _ _ -> callback(unbox data); null)
      settings.dataType <- "jsonp"
      Globals.jQuery.ajax("http://search.twitter.com/search.json?q=" + searchText, settings)   
   
   let updateTweets map =
      clearMap()
      query (unbox <| jq?searchText._val()) showTweets

   member __.Setup() =
      //Doesn't work under localhost... might work when hosted?
      map.setCenter(google.maps.LatLng.Create(51.5171,0.1026))
   
      let initialQuery = "%23fsharp"
      let t = jq?searchText._val initialQuery |> ignore
      let searchButton = jq?searchButton
      let result = searchButton.click (fun _ -> updateTweets map |> ignore; null)
      ignore result
      updateTweets map

let main() =
   Globals.onload <- fun _ -> Program().Setup() :> obj

do Runtime.Run()
