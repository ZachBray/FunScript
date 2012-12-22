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