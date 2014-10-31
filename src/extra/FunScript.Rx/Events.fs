[<FunScript.JS>]
module FunScript.Rx.Events

open FunScript

/// Implemented using an RxJs subject
type Event<'a>() =
    [<JSEmitInline("{0}")>]
    member private __.PublishImpl() : IEvent<'a> = failwith "JavaScript only"
    [<JSEmitInline("{0}.onNext({1})")>]
    member __.Trigger(x : 'a) : unit = failwith "JavaScript only"
    [<JSEmitInline("{0}")>]
    member __.Publish = __.PublishImpl()
    [<JSEmitInline("new Rx.Subject()")>]
    static member Create() : Event<'a> = failwith "JavaScript only"