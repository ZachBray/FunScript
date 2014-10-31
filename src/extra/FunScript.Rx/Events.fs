[<FunScript.JS>]
module FunScript.Rx.Events

open FunScript

/// Implemented using an RxJs subject
type Event<'a>() =
    [<JSEmitInline("{0}.onNext({1})")>]
    member __.Trigger(x : 'a) : unit = failwith "JavaScript only"
    member __.Publish 
        with [<JSEmitInline("{0}.asObservable()")>] get () = failwith "JavaScript only"
    [<JSEmitInline("new Rx.Subject()")>]
    static member Create() : Event<'a> = failwith "JavaScript only"