// --------------------------------------------------------------------------------------
// Helper functions & JavaScript runtime for Type Provider mappings
// --------------------------------------------------------------------------------------

module private FunScript.Data.Utils

open FunScript
open Microsoft.FSharp.Quotations

let undef() = failwith "!"

let (|SpecificConstructor|_|) templateParameter = 
  match templateParameter with
  | Patterns.NewObject(cinfo1, _) -> (function
      | Patterns.NewObject(cinfo2, args) 
            when cinfo1.DeclaringType.FullName = cinfo2.DeclaringType.FullName ->
//            when cinfo1.MetadataToken = cinfo2.MetadataToken ->
          Some(cinfo1, args)
      | _ -> None)
  | _ -> invalidArg "templateParameter" "Unrecognized quotation: Must be NewObject."

[<JS; JSEmit("return {0}==null;")>]
let isNull x : bool = false

[<JS>]
let emptyIfNull (list:_ list) = 
   if isNull list then List.empty
   else list

type XMLHttpRequest = 
  abstract ``open`` : string * string -> unit
  abstract setRequestHeader : string * string -> unit
  abstract send : string -> unit

  abstract onreadystatechange : (unit -> unit) with get, set
  abstract readyState : float
  abstract responseText : string 

[<JS; JSEmit("""
    var res;
    if (window.XDomainRequest) {
      res = new XDomainRequest();
      res.setRequestHeader = function (header, value) { };
      res.onload = function () {
        res.readyState = 4;
        res.status = 200;
        res.onreadystatechange();
      };
    }
    else if (window.XMLHttpRequest)
      res = new XMLHttpRequest();
    else
      res = new ActiveXObject("Microsoft.XMLHTTP");
    res.get_onreadystatechange = function() { return res.onreadystatechange; }
    res.set_onreadystatechange = function(a) { res.onreadystatechange = a; }
    res.get_readyState = function() { return res.readyState; }
    res.get_responseText = function() { return res.responseText; }
    return res;""")>]
let newXMLHttpRequest() : XMLHttpRequest = failwith "never"

[<JS; JSEmit("return encodeURIComponent({0});")>]
let encodeURIComponent(s:string) : string = failwith "never"

[<JS; JSEmit("return $.getJSON({0}, {1});")>]
let getJSON (url:string, callback : string -> unit) : unit = failwith "never"