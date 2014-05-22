// ---
// header: Asynchronous programming
// tagline: Use the full power of F# asynchronous workflows
// ---

[<ReflectedDefinition>]
module Program

open FunScript
open FunScript.TypeScript
open System.Threading

// ----------------------------------------------------------------------------
// Mini implementation of some F# async primitives

type Async =
  static member AwaitJQueryEvent(f : ('T -> obj) -> JQuery) : Async<'T> = 
    Async.FromContinuations(fun (cont, econt, ccont) ->
      let named = ref None
      named := Some (f (fun v -> 
        (!named).Value.off() |> ignore
        cont v
        obj() )))
      
// ----------------------------------------------------------------------------
// Demo using mini F# async

let j (selector : string) = Globals.Dollar.Invoke(selector)
let (?) jq name = jq("#" + name)

let log(msg:string) =
   let tag = "<p>" + msg + "</p>"
   j?results.append [| tag :> obj |]
   |> ignore

let increment(n) = 
  async {
    do! Async.Sleep(1000)
    return n + 1 
  }

let rec worker(n) = 
  async { 
    let! v = Async.AwaitJQueryEvent(fun f -> j?next.click(fun x -> f x))
    let! n = increment(n)
    do log ("Count: " + n.ToString())
    return! worker(n)
  }

let main() = 
  async {
    let! x = Async.AwaitJQueryEvent(fun o -> j?document.ready(unbox<Function> o))
    let cts = new CancellationTokenSource()
    Async.StartImmediate(worker 0, cts.Token)
    j?stop.click(fun _ -> box <| cts.Cancel()) |> ignore
  } |> Async.StartImmediate


// ----------------------------------------------------------------------------

do Runtime.Run()