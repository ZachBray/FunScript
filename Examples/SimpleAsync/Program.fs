[<ReflectedDefinition>]
module Program

open FunJS
open FunJS.TypeScript
open FSharp.Http
open System.Threading

type j = FunJS.TypeScript.Api<"..\\Typings\\jquery.d.ts">
type lib = FunJS.TypeScript.Api<"..\\Typings\\lib.d.ts">

// ----------------------------------------------------------------------------
// Mini implementation of some F# async primitives

type Async =
  static member AwaitJQueryEvent(f : ('T -> unit) -> j.JQuery') : Async<'T> = 
    Async.FromContinuations(fun (cont, econt, ccont) ->
      let named = ref None
      named := Some (f (fun v -> 
        (!named).Value.off() |> ignore
        cont v)))
      
// ----------------------------------------------------------------------------
// Demo using mini F# async

let (?) (jq:j.JQueryStatic') name = jq.Invoke(name:string)

let log(msg:string) =
   let tag = "<p>" + msg + "</p>"
   (j.jQuery.Invoke "#results").append [| tag :> obj |]
   |> ignore

let increment(n) = 
  async {
    do! Async.Sleep(1000)
    return n + 1 
  }

let rec worker(n) = 
  async { 
    let! v = Async.AwaitJQueryEvent(j.jQuery?``#next``.click)
    let! n = increment(n)
    do log ("Count: " + n.ToString())
    return! worker(n)
  }

let main() = 
  async {
    let! x = Async.AwaitJQueryEvent(fun o -> j.jQuery?document.ready o)
    let cts = new CancellationTokenSource()
    Async.StartImmediate(worker 0, cts.Token)
    j.jQuery?``#stop``.click(fun _ -> cts.Cancel()) |> ignore
  } |> Async.StartImmediate

// ----------------------------------------------------------------------------
// Translate & compile (and host at http://localhost:8081 for easy testing)

do
  let source = <@@ main() @@> |> Compiler.compile
  let sourceWrapped = sprintf "(function () {\n%s\n})()" source
  let filename = "blackjack.js"
  System.IO.File.Delete filename
  System.IO.File.WriteAllText(filename, sourceWrapped)
  source |> printfn "%A"

  let root = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location) + "\\"
  let url = "http://localhost:8081/"
  let server = HttpServer.Start(url, root)

  printfn "\nHttp server running at: %s" url
  System.Console.ReadLine() |> ignore
  server.Stop()
  