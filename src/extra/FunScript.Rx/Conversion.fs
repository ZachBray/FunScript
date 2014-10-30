[<FunScript.JS>]
module FunScript.Rx.Conversion

open System
open FunScript

[<JSEmit("""var xs = {0};

if (xs.isRxJs) return xs;

var ys = Rx.Observable.create(function (observer) {
    
    var resources = xs.Subscribe({
        OnNext : observer.onNext,
        OnError : observer.onError,
        OnCompleted : observer.onCompleted
    });

    return resources.Dispose;
});

ys.isRxJs = true;

return ys;""")>]
let toRxJs(xs : IObservable<_>) = xs