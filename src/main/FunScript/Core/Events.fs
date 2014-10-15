[<FunScript.JS>]
module FunScript.Core.Events
open System

type IObserver<'T> =   
   abstract OnNext : value:'T -> unit
   abstract OnError : error:exn -> unit
   abstract OnCompleted : unit -> unit

type ActionObserver<'T> (onNext : 'T -> unit, onError : exn -> unit, onCompleted : unit -> unit) =    
   new(onNext : 'T -> unit) = ActionObserver<'T>(onNext, (fun e -> ()), (fun () -> ()))

   interface IObserver<'T> with
     member this.OnNext v = onNext v
     member this.OnError e = onError e
     member this.OnCompleted() = onCompleted()

type IObservable<'T> =
   abstract Subscribe : observer:IObserver<'T> -> IDisposable

type IDelegateEvent<'T> =
   abstract AddHandler : ('T -> unit) -> unit
   abstract RemoveHandler : ('T -> unit) -> unit

type IEvent<'T> =
   interface
      inherit IDelegateEvent<'T>
      inherit IObservable<'T>
   end

[<FunScript.JSEmit("return {0} !== {1};"); FunScript.Inline>]
let inline (!==) a b = failwith "never";

type ActionDisposable(f) =
   interface IDisposable with
      member this.Dispose() = f()

type private PublishedEvent<'T>(delegates:('T -> unit)[] ref) =
   let addHandler handler =
      let newArray = !delegates |> Array.append [|handler|]
      delegates := newArray

   let removeHandler handler = 
      let newArray = !delegates |> Array.filter (fun x -> handler !== x)
      delegates := newArray

   interface IEvent<'T> with      
      member this.AddHandler (f : 'T -> unit) = addHandler f
      member this.RemoveHandler (f : 'T -> unit) = removeHandler f

      member this.Subscribe(observer) =
         let f = observer.OnNext
         addHandler f
         let unsubscribe = fun () -> removeHandler f
         upcast new ActionDisposable(unsubscribe)

and Event<'T>() =
   let delegates = ref Array.empty<'T -> unit>   

   member this.Trigger args =
      !delegates |> Array.iter(fun f -> f(args))

   member this.Publish = PublishedEvent<'T>(delegates) :> IEvent<'T>

   static member Create() : 'T Event = Event<'T>()


// NOTE: Quotations don't allow object expressions, so this is just a reimplementation of the Observable module
// in the FSharp.Core library with actual types
module Observable =

    let inline protect f succeed fail =
        match (try Choice1Of2 (f ()) with e -> Choice2Of2 e) with
        | Choice1Of2 x -> (succeed x)
        | Choice2Of2 e -> (fail e)

    type private MapObservable<'T,'U>(f: 'T->'U, w: IObservable<'T>) =
        interface IObservable<'U> with
            member x.Subscribe(observer) =
                let newObserver =
                    ActionObserver<_>(
                        onNext =        (fun v -> protect (fun () -> f v) observer.OnNext observer.OnError),
                        onError =       (fun e -> observer.OnError(e)),
                        onCompleted =   (fun () -> observer.OnCompleted())
                    )
                w.Subscribe(newObserver)

    type private ChooseObservable<'T,'U>(f: 'T->'U option, w: IObservable<'T>) =
        interface IObservable<'U> with
            member x.Subscribe(observer) =
                let newObserver =
                    ActionObserver<_>(
                        onNext =        (fun v -> protect (fun () -> f v) (function None -> () | Some v2 -> observer.OnNext v2) observer.OnError),
                        onError =       (fun e -> observer.OnError(e)),
                        onCompleted =   (fun () -> observer.OnCompleted())
                    )
                w.Subscribe(newObserver)

    type private ScanObservable<'T,'U>(f: 'U->'T->'U, z: 'U, w: IObservable<'T>) =
        interface IObservable<'U> with
            member x.Subscribe(observer) =
                let state = ref z
                let newObserver =
                    ActionObserver<_>(
                        onNext =        (fun v -> let z = !state in protect (fun () -> f z v) (fun z -> state := z; observer.OnNext z) observer.OnError),
                        onError =       (fun e -> observer.OnError(e)),
                        onCompleted =   (fun () -> observer.OnCompleted())
                    )
                w.Subscribe(newObserver)

    type private PairwiseObservable<'T>(w: IObservable<'T>) =
        interface IObservable<'T*'T> with
            member x.Subscribe(observer) =
                let lastArgs = ref None
                let newObserver =
                    ActionObserver<_>(
                        onNext = (fun args2 ->
                            match !lastArgs with
                            | None -> ()
                            | Some args1 -> observer.OnNext(args1,args2)
                            lastArgs := Some args2),
                        onError =       (fun e -> observer.OnError(e)),
                        onCompleted =   (fun () -> observer.OnCompleted())
                    )
                w.Subscribe(newObserver)

    type private MergeObservable<'T>(w1: IObservable<'T>, w2: IObservable<'T>) =
        interface IObservable<'T> with
            member x.Subscribe(observer) =
                let stopped = ref false
                let completed1 = ref false
                let completed2 = ref false

                let observer1 =
                    ActionObserver<_>(
                        onNext =        (fun v -> if not !stopped then observer.OnNext v),
                        onError =       (fun e -> if not !stopped then stopped:=true; observer.OnError(e)),
                        onCompleted =   (fun () -> if not !stopped then completed1:=true; if !completed1 && !completed2 then stopped:=true; observer.OnCompleted())
                    )

                let observer2 =
                    ActionObserver<_>(
                        onNext =        (fun v -> if not !stopped then observer.OnNext v),
                        onError =       (fun e -> if not !stopped then stopped:=true; observer.OnError(e)),
                        onCompleted =   (fun () -> if not !stopped then completed2:=true; if !completed1 && !completed2 then stopped:=true; observer.OnCompleted())
                    )

                let h1 = w1.Subscribe(observer1)
                let h2 = w2.Subscribe(observer2)
                new ActionDisposable(fun () -> h1.Dispose(); h2.Dispose()) :> System.IDisposable


    [<CompiledName("Map")>]
    let map f (w: IObservable<'T>) =
        MapObservable(f, w) :> IObservable<_>
    
    [<CompiledName("Choose")>]
    let choose f (w: IObservable<'T>) =
        ChooseObservable(f, w) :> IObservable<_>
    
    [<CompiledName("Filter")>]
    let filter f (w: IObservable<'T>) =
        choose (fun x -> if f x then Some x else None) w

    [<CompiledName("Partition")>]
    let partition f (w: IObservable<'T>) =
        filter f w, filter (f >> not) w

    [<CompiledName("Scan")>]
    let scan f z (w: IObservable<'T>) =
        ScanObservable(f, z, w) :> IObservable<_>

    [<CompiledName("Add")>]
    let add f (w: IObservable<'T>) =
        w.Subscribe(ActionObserver(f)) |> ignore

    [<CompiledName("Subscribe")>]
    let subscribe (f: 'T -> unit) (w: IObservable<'T>) =
        w.Subscribe(ActionObserver(f))

    [<CompiledName("Pairwise")>]
    let pairwise (w : IObservable<'T>) : IObservable<'T * 'T> =
        PairwiseObservable(w) :> IObservable<_>

    [<CompiledName("Merge")>]
    let merge (w1: IObservable<'T>) (w2: IObservable<'T>) =
        MergeObservable(w1, w2) :> IObservable<_>

    [<CompiledName("Split")>]
    let split (f : 'T -> Choice<'U1,'U2>) (w: IObservable<'T>) =
        choose (fun v -> match f v with Choice1Of2 x -> Some x | _ -> None) w,
        choose (fun v -> match f v with Choice2Of2 x -> Some x | _ -> None) w