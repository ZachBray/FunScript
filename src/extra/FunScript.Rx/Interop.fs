module FunScript.Rx.Interop

open System
open FunScript

[<JS>]
module Replacements =

    let choose f xs =
        xs |> Observable.map f
        |> Observable.filter Option.isSome
        |> Observable.map Option.get

    let pairwise xs =
        xs |> Observable.scanInit (None, None) (fun acc x ->
            match snd acc with
            | None -> None, Some x
            | Some y -> Some(y, x), Some x)
        |> Observable.choose fst

    let partition f xs =
        Observable.filter f xs, Observable.filter (not << f) xs

    [<JSEmitInline("{2}.scan({1}, {0})")>]
    let scan f seed xs = failwith "JavaScript only"

    let split f xs =
        Observable.choose (fun x ->
            match f x with
            | Choice1Of2 y -> Some y
            | _ -> None) xs,
        Observable.choose (fun x ->
            match f x with
            | Choice2Of2 y -> Some y
            | _ -> None) xs
            
    [<JSEmitInline("{0}.subscribe({1})")>]
    let add (evt : IEvent<_>) callback : unit = failwith "JavaScript only"
            
    [<JSEmitInline("{ Dispose: {0}.subscribe({1}).dispose }")>]
    let subscribe (evt : IEvent<_>) callback : IDisposable = failwith "JavaScript only"
            
    [<JSEmitInline("{0}.onNext({1})")>]
    let onNext (observer : IObserver<'a>) (x : 'a) : unit = failwith "JavaScript only"
            
    [<JSEmitInline("{0}.onError({1})")>]
    let onError (observer : IObserver<_>) (ex : exn) : unit = failwith "JavaScript only"
            
    [<JSEmitInline("{0}.onCompleted()")>]
    let onCompleted (observer : IObserver<_>) : unit = failwith "JavaScript only"


let reactiveAssemblyName() =
    typeof<FSharp.Control.Reactive.Builders.ObservableBuilder>.Assembly.GetName().Name

let components() =
    [
        ExpressionReplacer.createModuleMapping 
            (reactiveAssemblyName()) "FSharp.Control.Reactive.ObservableModule"
            "FunScript.Rx" "FunScript.Rx.Observable"

        [
            ExpressionReplacer.createUnsafe 
                <@ Microsoft.FSharp.Control.Observable.add @>
                <@ FunScript.Rx.Observable.subscribe @>

            ExpressionReplacer.create 
                <@ Microsoft.FSharp.Control.Observable.choose @>
                <@ Replacements.choose @>

            ExpressionReplacer.create 
                <@ Microsoft.FSharp.Control.Observable.filter @>
                <@ FunScript.Rx.Observable.filter @>

            ExpressionReplacer.create 
                <@ Microsoft.FSharp.Control.Observable.map @>
                <@ FunScript.Rx.Observable.map @>

            ExpressionReplacer.create 
                <@ Microsoft.FSharp.Control.Observable.merge @>
                <@ FunScript.Rx.Observable.merge @>

            ExpressionReplacer.create 
                <@ Microsoft.FSharp.Control.Observable.pairwise @>
                <@ Replacements.pairwise @>

            ExpressionReplacer.create 
                <@ Microsoft.FSharp.Control.Observable.partition @>
                <@ Replacements.partition @>

            ExpressionReplacer.create 
                <@ Microsoft.FSharp.Control.Observable.scan @>
                <@ Replacements.scan @>

            ExpressionReplacer.create 
                <@ Microsoft.FSharp.Control.Observable.split @>
                <@ Replacements.split @>

            ExpressionReplacer.create 
                <@ Microsoft.FSharp.Control.Observable.subscribe @>
                <@ FunScript.Rx.Observable.subscribe @>

            ExpressionReplacer.createUnsafe 
                <@ fun () -> Event<_>() @> 
                <@ fun () -> Events.Event.Create() @>

            ExpressionReplacer.createUnsafe 
                <@ fun (evt : Event<_>, arg) -> evt.Trigger arg @> 
                <@ fun (evt : Events.Event<_>, arg) -> evt.Trigger arg @>

            ExpressionReplacer.createUnsafe 
                <@ fun (evt : Event<_>) -> evt.Publish @> 
                <@ fun (evt : Events.Event<_>) -> evt.Publish @>
                
            ExpressionReplacer.create
                <@ fun (evt : IEvent<_>) callback -> evt.Add callback @> 
                <@ Replacements.add @> 
                
            ExpressionReplacer.create
                <@ fun (evt : IEvent<_>) callback -> evt.Subscribe callback @>
                <@ Replacements.subscribe @>
                
            ExpressionReplacer.create
                <@ fun (observer : IObserver<_>) x -> observer.OnNext x @>
                <@ Replacements.onNext @>

            ExpressionReplacer.create
                <@ fun (observer : IObserver<_>) ex -> observer.OnError ex @>
                <@ Replacements.onError @>

            ExpressionReplacer.create
                <@ fun (observer : IObserver<_>) -> observer.OnCompleted() @>
                <@ Replacements.onCompleted @>
        ]
    ] |> List.concat