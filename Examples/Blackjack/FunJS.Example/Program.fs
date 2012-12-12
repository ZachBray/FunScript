[<ReflectedDefinition>]
module Program

open FunJS
open FunJS.TypeScript

// See https://github.com/borisyankov/DefinitelyTyped for more
type j = FunJS.TypeScript.Api< @"..\..\Typings\jquery.d.ts" >
type lib = FunJS.TypeScript.Api< @"..\..\Typings\lib.d.ts" >

type goo = FunJS.TypeScript.Api< @"..\..\Typings\google.maps.d.ts" >

// Selector operator
let (!) (str:string) = j.jQuery.Invoke' str

type AjaxSettings(onSuccess) =
   // TODO: this does not work:
   //    inherit j.JQueryAjaxSettings'(async = true, context = null)
   // so we have to manually set the fields.
   // Need to implement parasitic inheritance in FunJS.
   inherit j.JQueryAjaxSettings'() 
   override __.success (data,_,_) = onSuccess data

type Tweet =
 { from_user: string
   text: string }

type TwitterSearch =
 { results: Tweet[] }

let mutable map = Unchecked.defaultof<goo.google.maps.Map>
let markers = ref List.empty

let showTweets (map:goo.google.maps.Map) (search:TwitterSearch) =
   let tweetList =
      search.results |> Array.map (fun tweet ->
         "<p><b>" + tweet.from_user + "</b>&nbsp;" + tweet.text + "</p>")
      |> Array.map box
   (!"#tweets").children'().remove'() |> ignore
   (!"#tweets").append tweetList |> ignore
   
let clearMap() =
   markers.contents |> List.iter (fun (marker:goo.google.maps.Marker) ->
      marker.setMap(unbox<goo.google.maps.Map> null))
   markers := List.empty

let query searchText (callback:'a -> unit) =
   let settings = AjaxSettings(fun data -> callback(data :?> 'a))
   settings.dataType <- "jsonp"
   j.jQuery.ajax("http://search.twitter.com/search.json?q=" + searchText, settings)   
   
let updateTweets map =
   clearMap()
   query (unbox <| (!"#searchText").``val``()) (showTweets map)

let createMap() =
   let options = goo.google.maps.MapOptions'()
   options.mapTypeId <- goo.google.maps.MapTypeId.ROADMAP
   options.zoom <- 3.
   let element = lib.document.getElementById "map"
   goo.google.maps.Map(unbox element, options)

let setup() =
   //Doesn't work under localhost... might work when hosted?
   map <- createMap()
   map.setCenter(goo.google.maps.LatLng(51.5171,0.1026))
   
   let initialQuery = "%23fsharp"
   (!"#searchText").val'' initialQuery |> ignore
   // Odd stuff is going on here! The expression the reflected
   // definition seems to be generating is incorrect. Unless there
   // is some weird expression replacement going on.
   let searchButton = !"#searchButton"
   let result = searchButton.click'' (fun _ -> updateTweets map; null)
   ignore result
   updateTweets map   

let main() =
   lib.window.onload <- fun _ -> setup() :> obj

// Compile
let source = <@@ main() @@> |> Compiler.compileWithoutReturn 
let filename = "twitter-example.js"
System.IO.File.Delete filename
System.IO.File.WriteAllText(filename, source)
source|> printfn "%A"
System.Console.ReadLine() |> ignore