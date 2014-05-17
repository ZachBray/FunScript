[<FunScript.JS>]
module FunScript.BrowserAPI.DomEvents
open System
open FunScript.Core.Events

[<FunScript.JSEmit("{0}.addEventListener({1},{2});")>]
let private addEventListener (element:obj) (eventName:string) (handler:'T -> unit) : unit = failwith "never"

[<FunScript.JSEmit("{0}.removeEventListener({1},{2});")>]
let private removeEventListener (element:obj) (eventName:string) (handler:'T -> unit) : unit = failwith "never"

/// A wrapper around DOM Events
type DomEvent<'T> (element, eventName) =   
   interface IEvent<'T> with
      member this.Subscribe (observer:IObserver<'T>) : IDisposable =
         let f = observer.OnNext
         addEventListener element eventName f
         let unsubscribe = fun() -> removeEventListener element eventName f
         upcast new ActionDisposable(unsubscribe)
      member this.AddHandler (f : 'T -> unit) = addEventListener element eventName f
      member this.RemoveHandler (f : 'T -> unit) = removeEventListener element eventName f

