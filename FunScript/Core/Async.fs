[<FunScript.JS>]
module FunScript.Core.Async

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
   { StackCounter : int ref 
     ExceptionCont : string -> unit
     CancelledCont : string -> unit 
     CancellationToken : CancellationToken }

type AsyncParams<'T> = 
   { Cont : 'T -> unit
     Aux : AsyncParamsAux }

type Async<'T> = Cont of (AsyncParams<'T> -> unit)

[<FunScript.JSEmit("return setTimeout({0}, {1});")>]
let setTimeout (handler:unit -> unit, milliseconds:float) = failwith "never"
  
let private protectedCont f = Cont (fun args ->
   args.Aux.CancellationToken.ThrowIfCancellationRequested()
   incr args.Aux.StackCounter
   if !args.Aux.StackCounter > 1000 then // TODO: Make this a parameter (this is pretty arbitrary)
      args.Aux.StackCounter := 0
      setTimeout((fun () -> f args), 1.0)
   else f args )

//let private incrStack  
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

   member x.While(cond, body) = 
      let x = x
      let rec loop() = 
         if cond() then x.Bind(body, loop)
         else x.Zero()
      loop()

   member x.Combine(work1, work2) = 
      x.Bind(work1, fun () -> work2)

   member x.For(seq:seq<_>, body) = 
      let en = seq.GetEnumerator()
      let x = x
      let rec loop() = 
         if en.MoveNext() then x.Bind(body en.Current, loop)
         else x.Zero()
      loop()

let async = AsyncBuilder()

type Async =
   static member FromContinuations(f) = protectedCont <| fun k ->
      f (k.Cont, k.Aux.ExceptionCont, k.Aux.CancelledCont)

   static member StartImmediate(workflow:Async<unit>, ?cancellationToken) =
      let token = defaultArg cancellationToken { Cell = None }
      let (Cont f) = workflow
      let aux = { StackCounter = ref 0; ExceptionCont = ignore; 
                  CancelledCont = ignore; CancellationToken = token }
      f { Cont = ignore; Aux = aux }

   static member Sleep(milliseconds:int) = 
      Async.FromContinuations(fun (cont, econt, ccont) ->
         setTimeout((fun _ -> cont()), float milliseconds) |> ignore )

(*
   static member AwaitJQueryEvent(f : ('T -> unit) -> j._JQuery) : Async<'T> = 
      Async.FromContinuations(fun (cont, econt, ccont) ->
         let named = ref None
         named := Some (f (fun v -> 
            (!named).Value.off() |> ignore
            cont v)))
*)
