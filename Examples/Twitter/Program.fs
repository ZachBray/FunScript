[<ReflectedDefinition>]
module Program

open FunScript
open FunScript.TypeScript


// See https://github.com/borisyankov/DefinitelyTyped for more
type ts = FunScript.TypeScript.Api< 
               @"../Typings/jquery.d.ts
                 ../Typings/google.maps.d.ts
                 ../Typings/lib.d.ts" >

type gmaps = ts.google.maps

type Tweet =
 { from_user: string
   text: string }

type TwitterSearch =
   { results: Tweet[] }

type Program() =
   let doc = ts.jQuery

   // Selector operator
   let (?) (jQ:ts.JQueryStatic) (str:string) = jQ.Invoke str

   let map = 
      let mapElement = ts.document.getElementById("map")
      ts.google.maps.Map.CreateInstance(unbox mapElement)

   let mutable markers = List.empty

   let showTweets (search:TwitterSearch) =
      let tweetList =
         search.results |> Array.map (fun tweet ->
            "<p><b>" + tweet.from_user + "</b>&nbsp;" + tweet.text + "</p>")
         |> Array.map box
      doc?``#tweets``.children().remove() |> ignore
      doc?``#tweets``.append tweetList |> ignore
   
   let clearMap() =
      markers |> List.iter (fun (marker:ts.google.maps.Marker) ->
         marker.setMap(unbox<ts.google.maps.Map> null))
      markers <- List.empty

   let query searchText (callback:'a -> unit) =
      let settings = ts.JQueryAjaxSettings()
      settings.set_success(fun data _ _ -> callback(unbox data))
      settings.dataType <- "jsonp"
      ts.jQuery.ajax("http://search.twitter.com/search.json?q=" + searchText, settings)   
   
   let updateTweets map =
      clearMap()
      query (unbox <| doc?``#searchText``.``val``()) showTweets

   member __.Setup() =
      //Doesn't work under localhost... might work when hosted?
      map.setCenter(gmaps.LatLng.CreateInstance(51.5171,0.1026))
   
      let initialQuery = "%23fsharp"
      doc?``#searchText``.``val`` initialQuery |> ignore
      let searchButton = doc?``#searchButton``
      let result = searchButton.click (fun _ -> updateTweets map)
      ignore result
      updateTweets map

let main() =
   ts.onload <- fun _ -> Program().Setup() :> obj

do Runtime.Run(components=Interop.Components.all)
