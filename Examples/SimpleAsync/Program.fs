[<ReflectedDefinition>]
module Program

open FunJS
open FunJS.TypeScript
open FSharp.Http

type j = FunJS.TypeScript.Api<"..\\Typings\\jquery.d.ts">
type lib = FunJS.TypeScript.Api<"..\\Typings\\lib.d.ts">

let log(msg:string) =
   let tag = "<p>" + msg + "</p>"
   (j.jQuery.Invoke "#results").append [| tag :> obj |]
   |> ignore

module FSharpAsync = 

  type CancellationToken = 
    private { Cell : option<bool ref> }
    member x.ThrowIfCancellationRequested() = 
      match x.Cell with
      | Some cell when !cell -> failwith "OperationCancelledException"
      | _ -> ()

  type CancellationTokenSource() =
    let token = { Cell = Some(ref false) }
    member x.Token = token
    member x.Cancel() = 
      token.Cell.Value := true

  type AsyncParamsAux =
    { ExceptionCont : string -> unit
      CancelledCont : string -> unit 
      CancellationToken : CancellationToken }

  type AsyncParams<'T> = 
    { Cont : 'T -> unit
      Aux : AsyncParamsAux }

  type Async<'T> = Cont of (AsyncParams<'T> -> unit)

  let private protectedCont f = Cont (fun args ->
    args.Aux.CancellationToken.ThrowIfCancellationRequested()
    f args )

  let private invokeCont k value = 
    k.Cont value

  type AsyncBuilder() =
    member x.Bind(Cont v:Async<'T>, f:'T -> Async<'R>) : Async<'R> = protectedCont <| fun k -> 
      let cont (a:'T) =
        let (Cont r) = f a 
        r k
      v { Cont = cont; Aux = k.Aux }

    member x.Delay(f) = protectedCont <| fun k -> 
      let (Cont r) = f ()
      r k

    member x.Zero () = protectedCont <| fun k -> 
      invokeCont k ()

    member x.ReturnFrom(w) : Async<'T> = w
    member x.Return(v) : Async<'T> = protectedCont <| fun k -> 
      invokeCont k v

    member this.While(cond, body) = 
      let x = this
      let rec loop() = 
        if cond() then x.Bind(body, loop)
        else x.Zero()
      loop()

  let async = AsyncBuilder()
  
  type Async =
    static member FromContinuations(f) = protectedCont <| fun k ->
      f (k.Cont, k.Aux.ExceptionCont, k.Aux.CancelledCont)

    static member StartImmediate(workflow:Async<unit>, ?cancellationToken) =
      let token = defaultArg cancellationToken { Cell = None }
      let (Cont f) = workflow
      let aux = { ExceptionCont = ignore; CancelledCont = ignore; CancellationToken = token }
      f { Cont = ignore; Aux = aux }

    static member Sleep(milliseconds:int) = 
      Async.FromContinuations(fun (cont, econt, ccont) ->
        lib.setTimeout((fun _ -> cont()), float milliseconds) |> ignore )

    static member AwaitJQueryEvent(f : ('T -> unit) -> j._JQuery) : Async<'T> = 
      Async.FromContinuations(fun (cont, econt, ccont) ->
        (f (fun v -> cont v)) |> ignore)
      
open FSharpAsync

let asyncFoo() = async {
    do! Async.Sleep(1000)
    return "hi" 
  }
let asyncMain() = async { 
    while true do 
      let! msg = asyncFoo()
      let! v = Async.AwaitJQueryEvent((j.jQuery.Invoke "#next").click)
      do log msg
  }

let main() =   
   (j.jQuery.Invoke "document").ready(fun _ -> 
      let cts = new CancellationTokenSource()
      let work = asyncMain()
      Async.StartImmediate(work, cts.Token)

      (j.jQuery.Invoke "#stop").click(fun _ ->
        cts.Cancel() )
    )

do
  // Compile
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
  