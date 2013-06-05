[<ReflectedDefinition>]
module Program

open FunScript
open FunScript.TypeScript
open System.Threading

type j = FunScript.TypeScript.Api<"..\\Typings\\jquery.d.ts">
type lib = FunScript.TypeScript.Api<"..\\Typings\\lib.d.ts">

// ----------------------------------------------------------------------------
// Mini implementation of some F# async primitives

type Async =
  static member AwaitJQueryEvent(f : ('T -> obj) -> j.JQuery) : Async<'T> = 
    Async.FromContinuations(fun (cont, econt, ccont) ->
      let named = ref None
      named := Some (f (fun v -> 
        (!named).Value.off() |> ignore
        cont v
        obj() )))
      
// ----------------------------------------------------------------------------
// Demo using mini F# async

let (?) (jq:j.JQueryStatic) name = jq.Invoke("#" + name)

let log(msg:string) =
   let tag = "<p>" + msg + "</p>"
   j.jQuery?results.append [| tag :> obj |]
   |> ignore

let increment(n) = 
  async {
    do! Async.Sleep(1000)
    return n + 1 
  }

let rec worker(n) = 
  async { 
    let! v = Async.AwaitJQueryEvent(fun f -> j.jQuery?next.click(f))
    let! n = increment(n)
    do log ("Count: " + n.ToString())
    return! worker(n)
  }

let main() = 
  async {
    let! x = Async.AwaitJQueryEvent(fun o -> j.jQuery?document.ready o)
    let cts = new CancellationTokenSource()
    Async.StartImmediate(worker 0, cts.Token)
    j.jQuery?stop.click(fun _ -> box <| cts.Cancel()) |> ignore
  } |> Async.StartImmediate


// ----------------------------------------------------------------------------

do Runtime.Run(components=Interop.Components.all)