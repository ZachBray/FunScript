[<RequireQualifiedAccess; FunScript.JS>]
module FunScript.Rx.Observable

open System
open System.Collections.Generic
open System.Reactive
open System.Reactive.Linq
open FunScript




/// Applies an accumulator function over an observable sequence, returning the 
/// result of the aggregation as a single element in the result sequence
[<JSEmitInline("{1}.aggregate({0})")>]
let aggregate accumulator source = 
    Observable.Aggregate(source, Func<_,_,_> accumulator )

/// Determines whether all elements of and observable satisfy a predicate
[<JSEmitInline("{1}.all({0})")>]
let all pred source =
    Observable.All(source, pred )


/// Returns the observable sequence that reacts first
[<JSEmitInline("Rx.Observable.amb({1}, {0})")>]
let amb second first = Observable.Amb(first, second)


/// Propagates the observable sequence that reacts first
[<JSEmitInline("Rx.Observable.amb({0})")>]
let ambSeqImpl (source : array<IObservable<'T>>) = Observable.Amb( source )

let ambSeq (source : IEnumerable<IObservable<'T>>) = ambSeqImpl (Seq.toArray source)

//let ambSeq (source:seq<IObservable<'T>>) = Observable.Amb( source )
//
//
/// Propagates the observable sequence that reacts first
[<JSEmitInline("Rx.Observable.amb({0})")>]
let ambArray (source:IObservable<'T>[]) = Observable.Amb( source  )


/// Determines whether an observable sequence contains any elements
[<JSEmitInline("{0}.any()")>]
let any  (source:IObservable<'Source>) : IObservable<bool> = 
    Observable.Any(source)


/// Hides the identy of an observable sequence 
//let asObservable source : IObservable<'Source>=
//    Observable.AsObservable( source )
//
//
/// Binds an observable to generate a subsequent observable.
//let bind (f: 'T -> IObservable<'TNext>) (m: IObservable<'T>) = m.SelectMany(Func<_,_> f)
//
//
/// Lifts the values of f and m and applies f to m, returning an IObservable of the result.
//let apply f m = f |> bind (fun f' -> m |> bind (fun m' -> Observable.Return(f' m')))
// 
//
/// Matches when both observable sequences have an available value
//let both second first = Observable.And(first, second)   
//
// #region Buffers
//
//
[<JSEmitInline("{1}.buffer({0})")>]
let buffer (bufferClosingSelector:IObservable<'BufferClosing>) source = 
    Observable.Buffer(source, bufferClosingSelector)


/// Projects each element of an observable sequence into 
/// consequtive non-overlapping buffers based on a sequence of boundary markers
[<JSEmitInline("{1}.buffer({0})")>]
let bufferBounded (boundaries:IObservable<'BufferClosing>) source : IObservable<IList<'T>>= 
    Observable.Buffer(source, boundaries)


/// Projects each element of an observable sequence into 
/// consequtive non-overlapping buffers produced based on count information
[<JSEmitInline("{1}.bufferWithCount({0})")>]
let bufferCount (count:int) source = 
    Observable.Buffer(source, count)


/// Projects each element of an observable sequence into zero or more buffers
/// which are produced based on element count information
[<JSEmitInline("{2}.bufferWithCount({0}, {1})")>]
let bufferCountSkip (count:int) (skip:int) source = 
    Observable.Buffer(source,count, skip)


/// Projects each element of an observable sequence into 
/// consequtive non-overlapping buffers produced based on timing information
[<JSEmitInline("{1}.bufferWithTime({0})")>]
let bufferSpanImpl (timeSpan : float) (source : IObservable<'a>) = Observable.Buffer(source, TimeSpan.FromMilliseconds timeSpan)

let bufferSpan (timeSpan : TimeSpan) (source : IObservable<'a>) = bufferSpanImpl timeSpan.TotalMilliseconds source

//let bufferSpan (timeSpan:TimeSpan) source = 
//    Observable.Buffer(source, timeSpan)
//
//
/// Projects each element of an observable sequence into a buffer that goes
/// sent out when either it's full or a specific amount of time has elapsed
/// Analogy - A boat that departs when it's full or at its scheduled time to leave
[<JSEmitInline("{2}.bufferWithTimeOrCount({0}, {1})")>]
let bufferSpanCountImpl (timeSpan : float) (count : Int32) (source : IObservable<'a>) = 
    Observable.Buffer(source, TimeSpan.FromMilliseconds timeSpan, count)

let bufferSpanCount (timeSpan : TimeSpan) (count : Int32) (source : IObservable<'a>) = bufferSpanCountImpl timeSpan.TotalMilliseconds count source

//let bufferSpanCount (timeSpan:TimeSpan) (count:int) source = 
//    Observable.Buffer(source, timeSpan, count)
//
//
//
//
/// Projects each element of an observable sequence into zero of more buffers. 
/// bufferOpenings - observable sequence whose elements denote the opening of each produced buffer
/// bufferClosing - observable sequence whose elements denote the closing of each produced buffer
[<JSEmitInline("{2}.buffer({0}, {1})")>]
let bufferFork  ( bufferOpenings:IObservable<'BufferOpening>) 
                ( bufferClosingSelector: 'BufferOpening ->IObservable<'T> ) source = 
    Observable.Buffer( source, bufferOpenings,Func<_,_> bufferClosingSelector)


/// Projects each element of an observable sequence into 
/// zero or more buffers produced based on timing information
[<JSEmitInline("{2}.bufferWithTime({0}, {1})")>]
let bufferSpanShiftImpl (timeSpan : float) (timeShift : float) (source : IObservable<'a>) = Observable.Buffer(source, TimeSpan.FromMilliseconds timeSpan, TimeSpan.FromMilliseconds timeShift)

let bufferSpanShift (timeSpan : TimeSpan) (timeShift : TimeSpan) (source : IObservable<'a>) = bufferSpanShiftImpl timeSpan.TotalMilliseconds timeShift.TotalMilliseconds source

//let bufferSpanShift (timeSpan:TimeSpan) (timeShift:TimeSpan) source = 
//    Observable.Buffer(source, timeSpan, timeShift)
//
//
// #endregion
//
//
/// Converts the elements of the sequence to the specified type
[<JSEmitInline("{0}")>]
let cast<'CastType> (source) =
    Observable.Cast<'CastType>(source)


/// Uses selector to determine which source in sources to return,
/// choosing an empty sequence if no match is found
[<JSEmitInline("Rx.Observable.case({0}, {1})")>]
let case selector sources =
    Observable.Case( Func<_> selector, sources )


/// Uses selector to determine which source in sources to return,
/// choosing defaulSource if no match is found
//let caseDefault selector (defaulSource:IObservable<'Result>) (sources:IDictionary<'Value,IObservable<'Result>>) =
//    Observable.Case( Func<'Value> selector, sources, defaulSource )
//
//
/// Continues an observable sequence that is terminated
/// by an exception with the next observable sequence.
[<JSEmitInline("{1}.catch({0})")>]
let catch (second: IObservable<'T>) first =
    Observable.Catch(first, second) 


/// Continues an observable sequence that is terminated by an exception of
/// the specified type with the observable sequence produced by the handler.
[<JSEmitInline("{1}.catch({0})")>]
let catchWith handler source =
    Observable.Catch( source,Func<_,_> handler )


/// Continues an observable sequence that is terminated by an exception with the next observable sequence.
[<JSEmitInline("Rx.Observable.catch({0})")>]
let catchSeqImpl (sources : array<IObservable<'T>>) = Observable.Catch(sources)

let catchSeq (sources : IEnumerable<IObservable<'T>>) = catchSeqImpl (Seq.toArray sources)

//let catchSeq (sources:seq<IObservable<'T>>) =
//    Observable.Catch(sources)
//
//
/// Continues an observable sequence that is terminated by an exception with the next observable sequence.
[<JSEmitInline("Rx.Observable.catch({0})")>]
let catchArray (sources:IObservable<'T>[]) =
    Observable.Catch(sources)


/// Produces an enumerable sequence of consequtive (possibly empty) chunks of the source observable
//let chunkify<'Source> source : seq<IList<'Source>> = 
//    Observable.Chunkify<'Source>( source )
//
//
/// Concatenates the observable sequences obtained by applying the map for each element in the given enumerable 
[<JSEmitInline("{1}.selectMany({0})")>]
let collectImpl (map : 'Source -> IObservable<'Result>) (source : array<'Source>) = Observable.For( source, Func<'Source, IObservable<'Result>> map )

let collect (map : 'Source -> IObservable<'Result>) (source : IEnumerable<'Source>) = collectImpl map (Seq.toArray source)

//let collect   ( map )( source:seq<'Source> ) : IObservable<'Result> =
//    Observable.For( source, Func<'Source, IObservable<'Result>> map )
//
//
//  
/// Produces an enumerable sequence that returns elements collected/aggregated from the source sequence between consecutive iterations.
/// merge - Merges a sequence element with the current collector
/// newCollector - Factory to create a new collector object.
//let collectMerge source newCollector merge = 
//    Observable.Collect( source, Func<_> newCollector,Func<_,_,_> merge )
//
//
/// Produces an enumerable sequence that returns elements collected/aggregated from the source sequence between consecutive iterations.
/// merge - Merges a sequence element with the current collector
/// getNewCollector - Factory to replace the current collector by a new collector
/// getInitialCollector - Factory to create the initial collector object.
//let collectMergeInit getInitialCollector merge getNewCollector source =
//    Observable.Collect( source           , Func<_> getInitialCollector  , 
//                        Func<_,_,_> merge, Func<_,_> getNewCollector    )
//
// #region CombineLatest Functions
//
//
/// Merges the specified observable sequences into one observable sequence by 
/// emmiting a list with the latest source elements of whenever any of the 
/// observable sequences produces and element.
[<JSEmitInline("(Rx.Observable.combineLatest({0}, function () { return arguments; }))")>]
let combineLatest (source :seq<IObservable<'T>> ) : IObservable<IList<'T>> =
    Observable.CombineLatest( source )


/// Merges the specified observable sequences into one observable sequence by  applying the map
/// whenever any of the observable sequences produces and element.
[<JSEmitInline("(Rx.Observable.combineLatest({0}, function () { return arguments; }))")>]
let combineLatestArray (source :IObservable<'T>[] )  =
    Observable.CombineLatest( source )        


/// Merges the specified observable sequences into one observable sequence by  applying the map
/// whenever any of the observable sequences produces and element.
[<JSEmitInline("(Rx.Observable.combineLatest({1}, function () { return {0}(arguments); }))")>]
let combineLatestMap ( map : IList<'T>-> 'Result  )(source :seq<IObservable<'T>> )  =
    Observable.CombineLatest( source, Func<IList<'T>,'Result> map )


/// Concatenates the second observable sequence to the first observable sequence
/// upn the successful termination of the first 
[<JSEmitInline("{1}.concat({0})")>]
let concat (second: IObservable<'T>) (first: IObservable<'T>) =
    Observable.Concat(first, second)


/// Concatenates all observable sequences within the sequence as long as
/// the previous observable sequence terminated successfully 
[<JSEmitInline("Rx.Observable.concat({0})")>]
let concatSeqImpl (sources : array<IObservable<'T>>) = Observable.Concat(sources)

let concatSeq (sources : IEnumerable<IObservable<'T>>) = concatSeqImpl (Seq.toArray sources)

//let concatSeq (sources:seq<IObservable<'T>>) : IObservable<'T> =
//    Observable.Concat(sources)
//
//
/// Concatenates all of the specified  observable sequences as long as
/// the previous observable sequence terminated successfully 
[<JSEmitInline("Rx.Observable.concat({0})")>]
let concatArray (sources:IObservable<'T>[]) =
    Observable.Concat(sources)


/// Concatenates all of the inner observable sequences as long as
/// the previous observable sequence terminated successfully 
[<JSEmitInline("{0}.concatAll()")>]
let concatInner (sources: IObservable<IObservable<'T>>) =
    Observable.Concat( sources )


/// Concatenates all task results as long as
/// the previous taskterminated successfully
//let concatTasks(sources: IObservable<Tasks.Task<'T>>) =
//    Observable.Concat( sources )
//
//
/// Connects the observable wrapper to its source. All subscribed
/// observers will recieve values from the underlying observable
/// sequence as long as the connection is established.    
/// ( publish an Observable to get a ConnectableObservable )
//let connect ( source:Subjects.IConnectableObservable<_> ) =    
//    source.Connect()
//
//
/// Determines whether an observable sequence contains a specified 
/// element by using the default equality comparer.
[<JSEmitInline("{1}.contains({0})")>]
let contains value source =
    Observable.Contains( source, value )


/// Determines whether an observable sequence contains a 
/// specified element by using a specified EqualityComparer
//let containsCompare comparer value source =
//    Observable.Contains( source, value, comparer )
//
//
/// Counts the elements
[<JSEmitInline("{0}.count()")>]
let count source = 
    Observable.Count(source)


/// Returns an observable sequence containing an int that represents how many elements 
/// in the specified observable sequence satisfy a condition.
[<JSEmitInline("{1}.count({0})")>]
let countSatisfy predicate source = 
    Observable.Count( source, predicate )

///  Creates an observable sequence from a specified Subscribe method implementation.
[<JSEmitInline("Rx.Observable.create({0})")>]
let create subscribe =
    Observable.Create(Func<IObserver<'Result>,Action> subscribe)


/// Creates an observable sequence from a specified Subscribe method implementation.
[<JSEmit("return Rx.Observable.create(function (observer) {
    var resource = {0}(observer);
    return resource.Dispose;
});")>]
let createWithDisposable subscribe =
    Observable.Create(Func<IObserver<'Result>,IDisposable> subscribe)


/// Returns the elements of the specified sequence or the type parameter's default value 
/// in a singleton sequence if the sequence is empty.
[<JSEmitInline("{0}.defaultIfEmpty()")>]
let defaultIfEmpty    ( source:IObservable<'Source> ): IObservable<'Source> =
    Observable.DefaultIfEmpty( source )


/// Returns the elements of the specified sequence or the specified value in a singleton sequence if the sequence is empty.
[<JSEmitInline("{1}.defaultIfEmpty({0})")>]
let defaultIfEmptyIs (defaultValue:'Source )( source:IObservable<'Source> ) : IObservable<'Source> =
    Observable.DefaultIfEmpty( source, defaultValue )


/// Returns an observable sequence that invokes the specified factory function whenever a new observer subscribes.    
[<JSEmitInline("Rx.Observable.defer({0})")>]
let defer ( observableFactory: unit -> IObservable<'Result> ): IObservable<'Result> =
    Observable.Defer(Func<IObservable<'Result>> observableFactory )


/// Time shifts the observable sequence by the specified relative time duration.
/// The relative time intervals between the values are preserved.
[<JSEmitInline("{1}.delay({0})")>]
let delayImpl (dueTime : float) (source : IObservable<'Source>) = Observable.Delay(source, TimeSpan.FromMilliseconds dueTime)

let delay (dueTime : TimeSpan) (source : IObservable<'Source>) = delayImpl dueTime.TotalMilliseconds source

//let delay ( dueTime:TimeSpan ) ( source:IObservable<'Source> ): IObservable<'Source>=
//    Observable.Delay(source, dueTime)
//
//
/// Time shifts the observable sequence to start propagating notifications at the specified absolute time.
/// The relative time intervals between the values are preserved.
//let delayUnitl  ( dueTime:DateTimeOffset ) ( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.Delay(source, dueTime )
//    
//
/// Time shifts the observable sequence based on a delay selector function for each element.
[<JSEmitInline("{1}.delayWithSelector({0})")>]
let delayMap ( delayDurationSelector:'Source -> IObservable<'TDelay> )  ( source:IObservable<'Source> ): IObservable<'Source> =
    Observable.Delay( source, Func<'Source,IObservable<'TDelay>> delayDurationSelector)


/// Time shifts the observable sequence based on a subscription delay and a delay selector function for each element.
//let delayMapFilter  ( delayDurationSelector         : 'Source -> IObservable<'TDelay>)
//                    ( subscriptionDelay             : IObservable<'TDelay>)  
//                    ( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.Delay(source, subscriptionDelay, Func<'Source, IObservable<'TDelay>> delayDurationSelector)
//
//
/// Time shifts the observable sequence by delaying the subscription with the specified relative time duration.
//let delaySubscription ( dueTime:TimeSpan) ( source:IObservable<'Source> ): IObservable<'Source> =
//    Observable.DelaySubscription( source, dueTime )
//
//
/// Time shifts the observable sequence by delaying the subscription to the specified absolute time.
//let delaySubscriptionUntil ( dueTime:DateTimeOffset) ( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.DelaySubscription( source, dueTime )
//
//
/// Dematerializes the explicit notification values of an observable sequence as implicit notifications.
//let dematerialize source = 
//    Observable.Dematerialize(source)
//
//
/// Returns an observable sequence that only contains distinct elements 
[<JSEmitInline("{0}.distinct()")>]
let distinct ( source:IObservable<'Source> ) : IObservable<'Source> =
    Observable.Distinct( source )


/// Returns an observable sequence that contains only distinct elements according to the keySelector.
[<JSEmitInline("{1}.distinct({0})")>]
let distinctKey ( keySelector:'Source -> 'Key )( source:IObservable<'Source> ) : IObservable<'Source> =
    Observable.Distinct( source, Func<'Source,'Key> keySelector)


/// Returns an observable sequence that contains only distinct elements according to the comparer.
//let distinctCompare ( comparer:IEqualityComparer<'Source> )( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.Distinct( source, comparer )
//
//
/// Returns an observable sequence that contains only distinct elements according to the keySelector and the comparer.
//let distinctKeyCompare ( keySelector:'Source -> 'Key )( comparer:IEqualityComparer<'Key>)( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.Distinct( source, Func<'Source,'Key> keySelector, comparer )
//   
//
/// Returns an observable sequence that only contains distinct contiguous elements 
[<JSEmitInline("{0}.distinct()")>]
let distinctUntilChanged ( source:IObservable<'Source> ) : IObservable<'Source> =
    Observable.DistinctUntilChanged(source)


/// Returns an observable sequence that contains only distinct contiguous elements according to the keySelector.
[<JSEmitInline("{1}.distinctUntilChanged({0})")>]
let distinctUntilChangedKey ( keySelector:'Source -> 'Key )( source:IObservable<'Source> )  : IObservable<'Source> =
    Observable.DistinctUntilChanged( source, Func<'Source,'Key> keySelector )
    

/// Returns an observable sequence that contains only distinct contiguous elements according to the comparer.
//let distinctUntilChangedCompare ( comparer:IEqualityComparer<'Source> )( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.DistinctUntilChanged( source, comparer )
//
//
/// Returns an observable sequence that contains only distinct contiguous elements according to the keySelector and the comparer.
//let distinctUntilChangedKeyCompare  ( keySelector:'Source -> 'Key )( comparer:IEqualityComparer<'Key> )( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.DistinctUntilChanged( source, Func<'Source,'Key> keySelector, comparer )
//
//
/// Returns the element at a specified index in a sequence.
[<JSEmitInline("{1}.elementAt({0})")>]
let elementAt  ( index:int ) ( source:IObservable<'Source>): IObservable<'Source> =
    Observable.ElementAt( source, index )


/// Returns the element at a specified index in a sequence or a default value if the index is out of range
[<JSEmitInline("{1}.elementAt({0})")>]
let elementAtOrDefault ( index:int )( source:IObservable<'Source> ) : IObservable<'Source> =
    Observable.ElementAt( source, index )


/// Returns an empty observable
[<JSEmitInline("Rx.Observable.empty()")>]
let empty<'T> = Observable.Empty<'T>()


/// Returns an empty Observable sequence
//let emptyWitness<'T>(witness:'T) :IObservable<'T> =
//         Observable.Empty( witness )
//
//
/// Determines whether two sequences are equal by comparing the elements pairwise.
//let equals ( first:IObservable<'Source>  )( second:IObservable<'Source> ) : IObservable<bool> =
//    Observable.SequenceEqual( first, second )
//
//
/// Determines whether two sequences are equal by comparing the elements pairwise using a specified equality comparer.
//let equalsCompare ( first:IObservable<'Source>  )( second:IObservable<'Source> )( comparer:IEqualityComparer<'Source>) : IObservable<bool> =
//    Observable.SequenceEqual( first, second )
//
//
/// Determines whether an observable and enumerable sequence are equal by comparing the elements pairwise.
//let equalsSeq ( first:IObservable<'Source>  )( second:seq<'Source>) : IObservable<bool> =
//    Observable.SequenceEqual( first, second )
//
//
/// Determines whether an observable and enumerable sequence are equal by comparing the elements pairwise using a specified equality comparer.
//let equalsSeqComparer ( first:IObservable<'Source>  )( second:seq<'Source> )( comparer:IEqualityComparer<'Source> ) : IObservable<bool> =
//    Observable.SequenceEqual( first, second )
//
//
[<JSEmitInline("Rx.Observable.throw({0})")>]
let error e = Observable.Throw e


/// Determines whether an observable sequence contains a specified value
/// which satisfies the given predicate
//let exists predicate source = 
//    Observable.Any(source, predicate)
//
//
/// Filters the observable elements of a sequence based on a predicate 
[<JSEmitInline("{1}.where({0})")>]
let filter  (predicate:'T->bool) (source:IObservable<'T>) = 
    Observable.Where( source, predicate )


/// Filters the observable elements of a sequence based on a predicate by 
/// incorporating the element's index
//let filteri (predicate:'T->int->bool) (source:IObservable<'T>)  = 
//    Observable.Where( source, predicate )
//
//
/// Invokes a specified action after the source observable sequence
/// terminates gracefully of exceptionally
[<JSEmitInline("{1}.finally({0})")>]
let finallyDo  finallyAction  source  =
    Observable.Finally( source, Action finallyAction ) 


/// Returns the first element of an observable sequence
[<JSEmitInline("{0}.first()")>]
let first (source:IObservable<'T>)  = 
    source.FirstAsync()


/// Returns the first element of an observable sequence
/// if it satisfies the predicate
[<JSEmitInline("{1}.first({0})")>]
let firstIf predicate (source:IObservable<'T>) =
    source.FirstAsync( predicate )


/// Projects each element of an observable sequence to an observable sequence 
/// and merges the resulting observable sequences into one observable sequenc
[<JSEmitInline("{1}.selectMany({0})")>]
let flatmap map source = 
    Observable.SelectMany(source, Func<'S,IObservable<'R>> map )


/// Projects each element of an observable sequence to an observable sequence by incorporating the 
/// element's index and merges the resulting observable sequences into one observable sequence.
//    let flatmapi map source = 
//        Observable.SelectMany(source,Func<'Source,int,seq<'Result>> map )
//

/// Projects each element of the source observable sequence to the other observable sequence 
/// and merges the resulting observable sequences into one observable sequence.
//let flatmapOther  ( other:IObservable<'Other> ) ( source:IObservable<'Source> ): IObservable<'Other> =
//    Observable.SelectMany( source, other )
//
//
/// Projects each element of an observable sequence to an enumerable sequence and concatenates 
/// the resulting enumerable sequences into one observable sequence.
//let flatmapSeq map source = 
//    Observable.SelectMany(source, Func<'Source,seq<'Result>> map)
//
//
/// Projects each element of an observable sequence to an enumerable sequence by incorporating the 
/// element's index and concatenates the resulting enumerable sequences into one observable sequence.
//    let flatmapSeqi map source = 
//        Observable.SelectMany(source, Func<'Source,int,seq<'Result>> map)
//
//    
//
/// Projects each element of an observable sequence to a task and merges all of the task results into one observable sequence.
//let flatmapTask  ( map ) ( source:IObservable<'Source> ) : IObservable<'Result> =
//    Observable.SelectMany( source, Func<'Source,Threading.Tasks.Task<'Result>> map ) 
//
//
/// Projects each element of an observable sequence to a task by incorporating the element's index 
/// and merges all of the task results into one observable sequence.
//    let flatmapTaski  ( map ) ( source:IObservable<'Source> ) : IObservable<'Result> =
//        Observable.SelectMany( source, Func<'Source,int,Threading.Tasks.Task<'Result>> map ) 
//
//
/// Applies an accumulator function over an observable sequence, returning the 
/// result of the fold as a single element in the result sequence
/// init is the initial accumulator value
//let fold accumulator init source =
//    Observable.Aggregate(source, init, Func<_,_,_> accumulator)
//
//
/// Applies an accumulator function over an observable sequence, returning the 
/// result of the fold as a single element in the result sequence
/// init is the initial accumulator value, map is performed after the fold
//let foldMap accumulator init map source =
//    Observable.Aggregate(source, init,Func<_,_,_> accumulator,Func<_,_>  map )
//
//
/// Converts an Action-based .NET event to an observable sequence. Each event invocation is surfaced through an OnNext message in the resulting sequence.
/// For conversion of events conforming to the standard .NET event pattern, use any of the FromEventPattern overloads instead.
//let fromEvent ( addHandler )( removeHandler ) : IObservable<unit> =
//    Observable.FromEvent( Action<'Delegate> addHandler, Action<'Delegate> removeHandler )
//
//
/// Converts an generic Action-based .NET event to an observable sequence. Each event invocation is surfaced through an OnNext message in the resulting sequence.
/// For conversion of events conforming to the standard .NET event pattern, use any of the FromEventPattern overloads instead.
//let fromEventGeneric ( addHandler    : ('TEventArgs -> unit ) -> unit )
//                     ( removeHandler : ('TEventArgs -> unit ) -> unit ) : IObservable<'TEventArgs> =
//    Observable.FromEvent(Action<'TEventArgs->unit> addHandler, Action<'TEventArgs->unit> removeHandler )
//
//
/// Converts a .NET event to an observable sequence, using a conversion function to obtain the event delegate. 
/// Each event invocation is surfaced through an OnNext message in the resulting sequence.
/// For conversion of events conforming to the standard .NET event pattern, use any of the FromEventPattern functions instead.
//let fromEventConversion<'EventArgs, 'Delegate when 'EventArgs:> EventArgs>
//        ( conversion   : ('EventArgs -> unit ) -> 'Delegate )
//        ( addHandler   : ('Delegate  -> unit )              )
//        ( removeHandler: ('Delegate  -> unit )              ) = 
//    { 
//      new IObservable<'EventArgs> with
//        member this.Subscribe(observer:IObserver<_>) =
//            let handler = observer.OnNext |> conversion
//            addHandler handler
//            let remove () = removeHandler handler
//            { new IDisposable with member this.Dispose() = remove () }
//    }
//
//
/// Converts a .NET event to an observable sequence, using a supplied event delegate type. 
/// Each event invocation is surfaced through an OnNext message in the resulting sequence.
//let fromEventHandler<'EventArgs when 'EventArgs:> EventArgs>
//    ( addHandler    : EventHandler<_> -> unit )
//    ( removeHandler : EventHandler<_> -> unit )  =
//    {   
//        new IObservable<_> with
//            member this.Subscribe( observer:IObserver<_> ) =
//                let handler = EventHandler<_>( fun _ x -> observer.OnNext x ) 
//                addHandler handler
//                let remove () = removeHandler handler
//                {   new IDisposable with member this.Dispose() = remove ()  }
//    }
//
//
/// Generates an observable from an IEvent<_> as an EventPattern.
//let fromEventPattern eventName (target:obj) =
//    Observable.FromEventPattern( target, eventName )
//
//
/// Generates an observable sequence by running a state-driven loop producing the sequence's elements.
[<JSEmitInline("Rx.Observable.generate({3}, {0}, {1}, {2})")>]
let generate initialState condition iterator resultMap = 
    Observable.Generate(                            initialState, 
                            Func<'State,bool>      condition   , 
                            Func<'State,'State>   iterator    , 
                            Func<'State,'Result>  resultMap   )


/// Generates an observable sequence by running a state-driven and temporal loop producing the sequence's elements.
//let generateTimed( initialState:'State   )
//                 ( condition             )
//                 ( iterate               )
//                 ( resultMap             )
//                 ( timeSelector          ) : IObservable<'Result> =
//    Observable.Generate(                                initialState   , 
//                            Func<'State,bool>          condition       , 
//                            Func<'State,'State>       iterate          , 
//                            Func<'State,'Result>      resultMap        ,
//                            Func<'State,DateTimeOffset>timeSelector    )
//
//
/// Generates an observable sequence by running a state-driven and temporal loop producing the sequence's elements.
//let generateTimeSpan ( initialState:'State )( condition )( iterate )( resultMap )( genTime ) : IObservable<'Result> =
//    Observable.Generate( initialState, condition, iterate, Func<'State,'Result>resultMap, Func<'State,TimeSpan>genTime )
//
//
/// Returns an enumerator that enumerates all values of the observable sequence.
//let getEnumerator ( source ) : IEnumerator<_> =
//    Observable.GetEnumerator( source )
//
//
/// Groups the elements of an observable sequence according to a specified key selector function.
//let groupBy ( keySelector  )
//            ( source      : IObservable<'Source>   ) :  IObservable<IGroupedObservable<'Key,'Source>> =
//    Observable.GroupBy( source,Func<'Source,'Key>  keySelector )
//
//
/// Groups the elements of an observable sequence with the specified initial 
/// capacity according to a specified key selector function.
//    let groupByCapacity ( keySelector )
//                        ( capacity:int )
//                        ( source:IObservable<'Source> ) : IObservable<IGroupedObservable<'Key,'Source>> =
//        Observable.GroupBy( source, Func<'Source,'Key> keySelector, capacity)
//
//
/// Groups the elements of an observable sequence according to a specified key selector function and comparer.
//let groupByCompare ( keySelector  )
//                    ( comparer    : IEqualityComparer<_> )
//                    ( source      : IObservable<_>    ) : IObservable<IGroupedObservable<_,_>> =
//    Observable.GroupBy( source, Func<_,_> keySelector, keySelector )
//
//
/// Groups the elements of an observable sequence and selects the resulting elements by using a specified function.
//let groupByElement  ( keySelector       )
//                    ( elementSelector   ) 
//                    ( source            ) : IObservable<IGroupedObservable<'Key,'Element>> =
//    Observable.GroupBy( source, Func<'Source,'Key>  keySelector, Func<'Source,'Element> elementSelector )
//
//
/// Groups the elements of an observable sequence with the specified initial capacity 
/// according to a specified key selector function and comparer.
//    let groupByCapacityCompare
//                ( keySelector                           )
//                ( capacity  : int                       )
//                ( comparer  : IEqualityComparer<'Key>   )
//                ( source    : IObservable<'Source>      )    : IObservable<IGroupedObservable<'Key,'Source>> =
//        Observable.GroupBy( source, Func<'Source,'Key> keySelector, capacity, comparer )
//    
//
/// Groups the elements of an observable sequence with the specified initial capacity
/// and selects the resulting elements by using a specified function.
//let groupByCapacityElement
//            ( keySelector           )
//            ( capacity       : int  )
//            ( elementSelector       )
//            ( source         : IObservable<'Source> ): IObservable<IGroupedObservable<'Key,'Element>> =
//    Observable.GroupBy( source, Func<'Source,'Key> keySelector, Func<'Source,'Element>  elementSelector )
//
//
/// Groups the elements of an observable sequence according to a specified key selector function 
/// and comparer and selects the resulting elements by using a specified function.
//let groupByCompareElement
//            ( keySelector      )
//            ( comparer       : IEqualityComparer<'Key>       )
//            ( elementSelector  )
//            ( source         : IObservable<'Source>           ): IObservable<IGroupedObservable<'Key,'Element>> =
//    Observable.GroupBy( source,Func<'Source,'Key>  keySelector, Func<'Source,'Element> elementSelector )
//
//
//
/// Groups the elements of an observable sequence with the specified initial capacity according to a 
/// specified key selector function and comparer and selects the resulting elements by using a specified function.
//    let groupByCapacityCompareElement
//                ( keySelector      )
//                ( capacity        : int                      )
//                ( comparer        : IEqualityComparer<'Key> ) 
//                ( elementSelector  ) 
//                ( source          : IObservable<'Source>    ) : IObservable<IGroupedObservable<'Key,'Element>> =
//        Observable.GroupBy( source, Func<'Source,'Key>    keySelector, Func<'Source,'Element> elementSelector, capacity, comparer )
//
//
///  Groups the elements of an observable sequence according to a specified key selector function.
///  A duration selector function is used to control the lifetime of groups. When a group expires, 
///  it receives an OnCompleted notification. When a new element with the same
///  key value as a reclaimed group occurs, the group will be reborn with a new lifetime request.
//let groupByUntil( keySelector )
//                ( durationSelector )
//                ( source:IObservable<'Source> ) : IObservable<IGroupedObservable<'Key,'Source>> =
//    Observable.GroupByUntil( source, Func<'Source,'Key>keySelector,Func<IGroupedObservable<'Key,'Source>,IObservable<'TDuration>> durationSelector )
//
//
/// Groups the elements of an observable sequence with the specified initial capacity according to a specified key selector function.
/// A duration selector function is used to control the lifetime of groups. When a group 
/// expires, it receives an OnCompleted notification. When a new element with the same
/// key value as a reclaimed group occurs, the group will be reborn with a new lifetime request.
//    let groupByCapacityUntil
//                    ( keySelector     )
//                    ( capacity        : int )  
//                    ( durationSelector )
//                    ( source          : IObservable<'Source> ): IObservable<IGroupedObservable<'Key,'Source>> =
//        Observable.GroupByUntil( source, Func<'Source,'Key> keySelector,Func<IGroupedObservable<'Key,'Source>,IObservable<'TDuration>>  durationSelector, capacity)
//
//
/// Groups the elements of an observable sequence according to a specified key selector function and comparer.
/// A duration selector function is used to control the lifetime of groups. When a group expires, 
/// it receives an OnCompleted notification. When a new element with the same
/// key value as a reclaimed group occurs, the group will be reborn with a new lifetime request.
//let groupByComparerUntil
//                ( keySelector)
//                ( comparer: IEqualityComparer<'Key> ) 
//                ( durationSelector )
//                ( source:IObservable<'Source> ) : IObservable<IGroupedObservable<'Key,'Source>> =
//    Observable.GroupByUntil( source, Func<'Source,'Key>  keySelector, Func<IGroupedObservable<'Key,'Source>,IObservable<'TDuration>> durationSelector, comparer )
//
//
/// Groups the elements of an observable sequence according to a specified key selector function 
/// and selects the resulting elements by using a specified function.
/// A duration selector function is used to control the lifetime of groups. When a group expires, 
/// it receives an OnCompleted notification. When a new element with the same
/// key value as a reclaimed group occurs, the group will be reborn with a new lifetime request.
//let groupByElementUntil
//                ( keySelector )
//                ( elementSelector )
//                ( durationSelector)
//                ( source:IObservable<'Source> ): IObservable<IGroupedObservable<'Key,'Element>> =
//    Observable.GroupByUntil( source, Func<'Source,'Key> keySelector, Func<'Source,'Element>elementSelector, Func<IGroupedObservable<'Key,'Element>,IObservable<'TDuration>>  durationSelector )
//
//
/// Groups the elements of an observable sequence with the specified initial capacity according to a specified key selector function and comparer.
/// A duration selector function is used to control the lifetime of groups. When a group expires, it receives an OnCompleted notification. When a new element with the same
/// key value as a reclaimed group occurs, the group will be reborn with a new lifetime request.
//    let groupByCapacityComparerUntil    
//                        ( keySelector      )
//                        ( durationSelector ) 
//                        ( capacity : int   ) 
//                        ( comparer : IEqualityComparer<'Key> )
//                        ( source   : IObservable<'Source>    ) : IObservable<IGroupedObservable<'Key,'Source>> =
//        Observable.GroupByUntil(    source, 
//                                    Func<'Source,'Key> keySelector, 
//                                    Func<IGroupedObservable<'Key,'Source>,IObservable<'TDuration>> durationSelector, 
//                                    capacity, 
//                                    comparer                    )
//
//
/// Groups the elements of an observable sequence with the specified initial capacity according to a specified key 
/// selector function and selects the resulting elements by using a specified function.
/// A duration selector function is used to control the lifetime of groups. When a group 
/// expires, it receives an OnCompleted notification. When a new element with the same
/// key value as a reclaimed group occurs, the group will be reborn with a new lifetime request.
//    let groupByCapacityElementUntil    
//                        ( keySelector      )
//                        ( capacity        : int )                   
//                        ( elementSelector   )
//                        ( durationSelector )
//                        ( source          : IObservable<'Source> ) : IObservable<IGroupedObservable<'Key,'Element>> =
//        Observable.GroupByUntil( source, Func<'Source,'Key>keySelector, Func<'Source,'Element>elementSelector, Func<IGroupedObservable<'Key,'Element>,IObservable<'TDuration>> durationSelector, capacity )
//
//
/// Groups the elements of an observable sequence according to a specified key selector function and 
/// comparer and selects the resulting elements by using a specified function.
/// A duration selector function is used to control the lifetime of groups. When a group expires,
/// it receives an OnCompleted notification. When a new element with the same
/// key value as a reclaimed group occurs, the group will be reborn with a new lifetime request.
//let groupByComparerElementUntil
//                ( keySelector )
//                ( comparer:Collections.Generic.IEqualityComparer<'Key>) 
//                ( elementSelector )
//                ( durationSelector )
//                ( source:IObservable<'Source> ) : IObservable<IGroupedObservable<'Key,'Element>> =
//    Observable.GroupByUntil( source, Func<'Source,'Key> keySelector, Func<'Source,'Element>elementSelector, Func<IGroupedObservable<'Key,'Element>,IObservable<'TDuration>>durationSelector, comparer )
//
//
/// Groups the elements of an observable sequence with the specified initial capacity according to a specified
/// key selector function and comparer and selects the resulting elements by using a specified function.
/// A duration selector function is used to control the lifetime of groups. When a group expires, it receives 
/// an OnCompleted notification. When a new element with the same
/// key value as a reclaimed group occurs, the group will be reborn with a new lifetime request.
//    let groupByCapacityComparerElementUntil
//                    ( keySelector )
//                    ( capacity:int )
//                    ( comparer:IEqualityComparer<'Key>)
//                    ( elementSelector )
//                    ( durationSelector )
//                    ( source:IObservable<'Source> ) : IObservable<IGroupedObservable<'Key,'Element>> =
//        Observable.GroupByUntil( source, Func<'Source,'Key>keySelector, Func<'Source,'Element>elementSelector, Func<IGroupedObservable<'Key,'Element>,IObservable<'TDuration>>durationSelector, capacity, comparer )
//
//
/// Correlates the elements of two sequences based on overlapping 
/// durations and groups the results
//let groupJoin   ( left        : IObservable<'Left>       ) 
//                ( right       : IObservable<'Right>      )  
//                ( leftselect  : 'Left -> IObservable<'a> ) 
//                ( rightselect : 'Right-> IObservable<'b> ) 
//                ( resultselect: 'Left -> IObservable<'Right>->'Result )  = 
//    Observable.GroupJoin(   left, right, 
//                            Func<'Left , IObservable<'a>>            leftselect  , 
//                            Func<'Right, IObservable<'b>>            rightselect , 
//                            Func<'Left , IObservable<'Right>,'Result>resultselect)
//
//
/// Creates an observable that calls the specified function (each time)
/// after an observer is attached to the observable. This is useful to 
/// make sure that events triggered by the function are handled. 
//let guard f (source:IObservable<'Args>) =  
//    {   
//        new IObservable<'Args> with  
//            member x.Subscribe( observer ) =  
//                let rm = source.Subscribe( observer ) in f() 
//                ( rm )
//    } 
//
//
/// Takes the first element of the observable sequence
//let head obs = Observable.FirstAsync(obs)
//
//
/// Returns and observable sequence that produces a value after each period
[<JSEmitInline("Rx.Observable.interval({0})")>]
let intervalImpl (period : float) = Observable.Interval(TimeSpan.FromMilliseconds period )

let interval (period : TimeSpan) = intervalImpl period.TotalMilliseconds

//let interval period = 
//    Observable.Interval( period )
//
//
/// Determines whether the given observable is empty 
[<JSEmitInline("{0}.isEmpty()")>]
let isEmpty source = source = Observable.Empty()


/// Invokes an action for each element in the observable sequence, and propagates all observer 
/// messages through the result sequence. This method can be used for debugging, logging, etc. of query 
/// behavior by intercepting the message stream to run arbitrary actions for messages on the pipeline.
[<JSEmitInline("{1}.subscribe({0})")>]
let iter ( onNext ) ( source:IObservable<'Source> ): IObservable<'Source> =
    Observable.Do( source, Action<'Source> onNext )
   

/// Invokes an action for each element in the observable sequence and invokes an action 
/// upon graceful termination of the observable sequence. This method can be used for debugging,
///  logging, etc. of query behavior by intercepting the message stream to run arbitrary
/// actions for messages on the pipeline.
[<JSEmitInline("{2}.subscribe({0}, {1})")>]
let iterEnd ( onNext )( onCompleted ) ( source:IObservable<'Source> ): IObservable<'Source> =
    Observable.Do( source, Action<'Source> onNext, Action onCompleted )   


/// Invokes an action for each element in the observable sequence and invokes an action upon 
/// exceptional termination of the observable sequence. This method can be used for debugging, 
/// logging, etc. of query behavior by intercepting the message stream to run arbitrary 
/// actions for messages on the pipeline.
[<JSEmitInline("{2}.subscribe({0}, {1})")>]
let iterError ( onNext)( onError ) ( source:IObservable<'Source> ): IObservable<'Source> =
    Observable.Do( source, Action<'Source> onNext, Action<exn> onError )   


/// Invokes an action for each element in the observable sequence and invokes an action 
/// upon graceful or exceptional termination of the observable sequence.
/// This method can be used for debugging, logging, etc. of query behavior by intercepting 
/// the message stream to run arbitrary actions for messages on the pipeline.
[<JSEmitInline("{3}.subscribe({0}, {1}, {2})")>]
let iterErrorEnd ( onNext )( onError ) ( onCompleted ) ( source:IObservable<'Source> ): IObservable<'Source> =
    Observable.Do( source, Action<'Source> onNext, Action<exn> onError, Action onCompleted )   
    

/// Invokes the observer's methods for each message in the source sequence.
/// This method can be used for debugging, logging, etc. of query behavior by intercepting 
/// the message stream to run arbitrary actions for messages on the pipeline.
[<JSEmitInline("{1}.subscribe({0})")>]
let iterObserver ( observer:IObserver<'Source> ) ( source:IObservable<'Source> ): IObservable<'Source> =
    Observable.Do( source,observer )   


/// Joins together the results from several patterns
//let joinWhen (plans:seq<Joins.Plan<'T>>): IObservable<'T> = 
//    Observable.When( plans )
//
//
/// Returns the last element of an observable sequence.
[<JSEmitInline("{0}.last()")>]
let last ( source:IObservable<'Source>) : IObservable<'Source> =
    Observable.LastAsync( source )


/// Returns the last element of an observable sequence that satisfies the condition in the predicate 
[<JSEmitInline("{1}.last({0})")>]
let lastIf  ( predicate ) ( source:IObservable<'Source> ) : IObservable<'Source> =
    Observable.LastAsync( source, Func<'Source,bool> predicate )


/// Returns an enumerable sequence whose enumeration returns the latest observed element in the source observable sequence.
/// Enumerators on the resulting sequence will never produce the same element repeatedly, 
/// and will block until the next element becomes available.
//let latest source = 
//    Observable.Latest( source )
//
//
/// Returns an observable sequence containing a int64 that represents 
/// the total number of elements in an observable sequence 
[<JSEmitInline("{0}.count()")>]
let longCount source = 
    Observable.LongCount(source)


/// Returns an observable sequence containing an int that represents how many elements 
/// in the specified observable sequence satisfy a condition.
[<JSEmitInline("{1}.count({0})")>]
let longCountSatisfy predicate source = 
    Observable.LongCount(source, predicate)    


/// Maps the given observable with the given function
[<JSEmitInline("{1}.select({0})")>]
let map f source = Observable.Select(source, Func<_,_>(f))   

/// Maps the given observable with the given function and the 
/// index of the element
let mapi (f:int -> 'Source -> 'Result) (source:IObservable<'Source>) =
    source 
    |> Observable.scan ( fun (i,_) x -> (i+1,Some(x))) (-1,None)
    |> Observable.map 
        (   function
            | i, Some(x) -> f i x
            | _, None    -> invalidOp "Invalid state"   )


/// Maps two observables to the specified function.
//let map2 f a b = apply (apply f a) b
//
//
/// Materializes the implicit notifications of an observable sequence as
/// explicit notification values
//let materialize source = 
//    Observable.Materialize( source )
//
//
/// Merges the two observables
[<JSEmitInline("{1}.merge({0})")>]
let merge (second: IObservable<'T>) (first: IObservable<'T>) = Observable.Merge(first, second)


/// Merges all the observable sequences into a single observable sequence.
[<JSEmitInline("Rx.Observable.merge({0})")>]
let mergeArray (sources:IObservable<'T>[]) =
    Observable.Merge(sources)


/// Merges elements from all inner observable sequences 
/// into a single  observable sequence.
[<JSEmitInline("{0}.mergeAll()")>]
let mergeInner (sources:IObservable<IObservable<'T>>) =
    Observable.Merge(sources)


/// Merges elements from all inner observable sequences 
/// into a single  observable sequence limiting the number of concurrent 
/// subscriptions to inner sequences
//let mergeInnerMax (maxConcurrent:int) (sources:IObservable<IObservable<'T>>) =
//    Observable.Merge(sources, maxConcurrent)
//
//
/// Merges an enumerable sequence of observable sequences into a single observable sequence.
[<JSEmitInline("Rx.Observable.merge({0})")>]
let mergeSeqImpl (sources : array<IObservable<'T>>) = Observable.Merge(sources)

let mergeSeq (sources : IEnumerable<IObservable<'T>>) = mergeSeqImpl (Seq.toArray sources)

//let mergeSeq (sources:seq<IObservable<'T>>) =
//    Observable.Merge(sources)
//
//
/// Merges an enumerable sequence of observable sequences into an observable sequence,
///  limiting the number of concurrent subscriptions to inner sequences.
//let mergeSeqMax (maxConcurrent:int)(sources:seq<IObservable<'T>>) =
//    Observable.Merge(sources, maxConcurrent)
//
//
/// Merge results from all source tasks into a single observable sequence
//let mergeTasks (sources:IObservable<Tasks.Task<'T>>) =
//    Observable.Merge(sources)
//
//
/// Returns the maximum element in an observable sequence.
//let maxOf (source:IObservable<'T>) = 
//    Observable.Max( source )
//
//
/// Returns an enumerable sequence whose sequence whose enumeration returns the 
/// most recently observed element in the source observable sequence, using 
/// the specified 
//let mostRecent initialVal source = 
//    Observable.MostRecent( source, initialVal )
//
//
/// Multicasts the source sequence notifications through the specified subject to 
/// the resulting connectable observable. Upon connection of the connectable 
/// observable, the subject is subscribed to the source exactly one, and messages
/// are forwarded to the observers registered with the connectable observable. 
/// For specializations with fixed subject types, see Publish, PublishLast, and Replay.
[<JSEmitInline("{1}.multicast({0})")>]
let multicast subject source =
    Observable.Multicast(source, subject)


/// Multicasts the source sequence notifications through an instantiated subject into
/// all uses of the sequence within a selector function. Each subscription to the 
/// resulting sequence causes a separate multicast invocation, exposing the sequence
/// resulting from the selector function's invocation. For specializations with fixed
/// subject types, see Publish, PublishLast, and Replay.
[<JSEmitInline("{2}.multicast({0}, {1})")>]
let multicastMap subjectSelector selector source  =
    Observable.Multicast(source,subjectSelector, selector)


/// Returns a non-terminating observable sequence, which can 
/// be used to denote an infinite duration (e.g. when using reactive joins).
[<JSEmitInline("Rx.Observable.never()")>]
let infinite() =
    Observable.Never()


/// Returns a non-terminating observable sequence, which can be 
/// used to denote an infinite duration (e.g. when using reactive joins).
//let neverWitness( witness ) =
//    Observable.Never( witness )
//
//
/// Returns an observable sequence whose enumeration blocks until the next
/// element in the source observable sequence becomes available. 
/// Enumerators  on the resulting sequence will block until the next
/// element becomes available.
//let next source = 
//    Observable.Next( source ) 
// 
//
/// Returns the sequence as an observable
[<JSEmitInline("Rx.Observable.fromArray({0})")>]
let ofSeqImpl (items : array<'Item>) = items.ToObservable()

let ofSeq (items : IEnumerable<'Item>) = ofSeqImpl (Seq.toArray items)

//let ofSeq<'Item>(items:'Item seq) : IObservable<'Item> =
//    {   
//        new IObservable<_> with
//            member __.Subscribe( observer:IObserver<_> ) =
//                for item in items do observer.OnNext item      
//                observer.OnCompleted()     
//                {   new IDisposable with member __.Dispose() = ()   }
//    }
//
//
/// Returns the sequence as an observable, using the specified scheduler to run the enumeration loop
//let ofSeqOn<'Item>(scheduler:Concurrency.IScheduler) (items:'Item seq) : IObservable<'Item> =
//    items.ToObservable(scheduler)
//
//
/// Wraps the source sequence in order to run its observer callbacks on the specified scheduler.
//let observeOn (scheduler:Concurrency.IScheduler) source =
//    Observable.ObserveOn( source, scheduler )
//
//
/// Wraps the source sequence in order to run its observer callbacks 
/// on the specified synchronization context.
//let observeOnContext (context:SynchronizationContext) source =
//    Observable.ObserveOn( source, context )
//
//
/// Filters the elements of an observable sequence based on the specified type
//let ofType source = 
//    Observable.OfType( source )
//
//
/// Concatenates the second observable sequence to the first observable sequence 
/// upon successful or exceptional termination of the first.
[<JSEmitInline("{1}.onErrorResumeNext({0})")>]
let onErrorConcat ( second:IObservable<'Source> ) ( first:IObservable<'Source> ) : IObservable<'Source> =
    Observable.OnErrorResumeNext( first, second )


/// Concatenates all of the specified observable sequences, even if the previous observable sequence terminated exceptionally.
[<JSEmitInline("Rx.Observable.onErrorResumeNext({0})")>]
let onErrorConcatArray ( sources:IObservable<'Source> [] ) : IObservable<'Source> =
    Observable.OnErrorResumeNext( sources )


/// Concatenates all observable sequences in the given enumerable sequence, even if the 
/// previous observable sequence terminated exceptionally.
[<JSEmitInline("Rx.Observable.onErrorResumeNext({0})")>]
let onErrorConcatSeqImpl (sources : array<IObservable<'Source>>) = Observable.OnErrorResumeNext( sources )

let onErrorConcatSeq (sources : IEnumerable<IObservable<'Source>>) = onErrorConcatSeqImpl (Seq.toArray sources)

//let onErrorConcatSeq ( sources:seq<IObservable<'Source>> ) : IObservable<'Source> =
//    Observable.OnErrorResumeNext( sources )
//
//    
/// Iterates through the observable and performs the given side-effect
[<JSEmitInline("{1}.doOnNext({0})")>]
let perform f source =
    let inner x = f x
    Observable.Do(source, inner)
 

/// Invokes the finally action after source observable sequence terminates normally or by an exception.
[<JSEmitInline("{1}.finally({0})")>]
let performFinally f source = Observable.Finally(source, Action f)


/// Returns a connectable observable sequence (IConnectableObsevable) that shares
/// a single subscription to the underlying sequence. This operator is a 
/// specialization of Multicast using a regular Subject
[<JSEmitInline("{0}.publish()")>]
let publish source = 
    Observable.Publish( source )


/// Returns a connectable observable sequence (IConnectableObsevable) that shares
/// a single subscription to the underlying sequence and starts with the value
/// initial. This operator is a specialization of Multicast using a regular Subject
//let publishInitial (initial:'Source) (source:IObservable<'Source>) = 
//    Observable.Publish( source, initial )
//
//
/// Returns an observable sequence that is the result of invoking 
/// the selector on a connectable observable sequence that shares a
/// a single subscription to the underlying sequence. This operator is a 
/// specialization of Multicast using a regular Subject
[<JSEmitInline("{1}.publish({0})")>]
let publishMap ( map:IObservable<'Source> -> IObservable<'Result> ) 
               ( source  :IObservable<'Source>            ) = 
    Observable.Publish( source, Func<IObservable<'Source>,IObservable<'Result>> map )


/// Returns an observable sequence that is the result of 
/// the map on a connectable observable sequence that shares a
/// a single subscription to the underlying sequence. This operator is a 
/// specialization of Multicast using a regular Subject
//let publishInitialMap  ( initial : 'Source  )
//                       ( map: IObservable<'Source> -> IObservable<'Result> ) 
//                       ( source  : IObservable<'Source> ) = 
//    Observable.Publish( source, Func<IObservable<'Source>,IObservable<'Result>> map, initial )
//
//
/// Returns an observable sequence that is the result of invoking 
/// the selector on a connectable observable sequence containing 
/// only the last notification This operator is a 
/// specialization of Multicast using a regular Subject
[<JSEmitInline("{0}.publishLast()")>]
let publishLast source = 
    Observable.PublishLast( source )


/// Returns an observable sequence that is the result of invoking 
/// the selector on a connectable observable sequence that shares a
/// a single subscription to the underlying sequence. This operator is a 
/// specialization of Multicast using a regular Subject
[<JSEmitInline("{1}.publishLast({0})")>]
let publishLastMap ( map: IObservable<'Source> -> IObservable<'Result> ) source  = 
    Observable.PublishLast( source , Func<IObservable<'Source>,IObservable<'Result>> map )


/// Creates a range as an observable
//let range start count = Observable.Range(start, count)
//
//
/// Reduces the observable
[<JSEmitInline("{1}.reduce({0})")>]
let reduce f source = Observable.Aggregate(source, Func<_,_,_> f)

 
/// Returns an observable that remains connected to the source as long
/// as there is at least one subscription to the observable sequence 
/// ( publish an Observable to get a ConnectableObservable )
[<JSEmitInline("{0}.refCount()")>]
let refCount ( source )=
    Observable.RefCount ( source )   


/// Repeats the observable sequence indefinitely.
//let repeat<'Source> ( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.Repeat<'Source>( source )
//
//
/// Repeats the observable sequence a specified number of times.
//let repeatCount<'Result> ( value:'Result )( repeatCount:int ) : IObservable<'Result> =
//    Observable.Repeat( value, repeatCount )
//
//
/// Generates an observable sequence that repeats the given element infinitely.
[<JSEmitInline("{0}.repeat()")>]
let repeatValue ( value:'Result ) : IObservable<'Result> =
    Observable.Repeat<'Result>( value )


/// Repeats the given observable sequence as long as the specified condition holds, where the
/// condition is evaluated after each repeated source is completed.
//let repeatWhile ( condition)( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.DoWhile( source, Func<bool> condition)
//
//
/// Returns a connectable observable sequence that shares a single subscription to the 
/// underlying sequence replaying all notifications.
[<JSEmitInline("{0}.replay()")>]
let replay ( source:IObservable<'Source>) : Subjects.IConnectableObservable<'Source> =        
    Observable.Replay( source )   


/// Returns a connectable observable sequence that shares a single subscription to the underlying sequence 
/// replaying notifications subject to a maximum element count for the replay buffer.
[<JSEmitInline("{1}.replay({0})")>]
let replayBuffer ( bufferSize:int )( source:IObservable<'Source>)  : Subjects.IConnectableObservable<'Source> =
        Observable.Replay( source )  


/// Returns an observable sequence that is the result of invoking the selector on a connectable observable 
/// sequence that shares a single subscription to the underlying sequence replaying all notifications.
[<JSEmitInline("{1}.replay({0})")>]
let replayMap ( map )( source:IObservable<'Source>)  : IObservable<'Result> =
        Observable.Replay( source, Func<IObservable<'Source>,IObservable<'Result>> map )  


/// Returns a connectable observable sequence that shares a single subscription to the underlying sequence 
/// replaying notifications subject to a maximum time length for the replay buffer.
//let replayWindow  ( window:TimeSpan ) ( source:IObservable<'Source>): Subjects.IConnectableObservable<'Source> =
//        Observable.Replay( source )  
//
//
/// Returns a connectable observable sequence that shares a single subscription to the underlying sequence
//  replaying notifications subject to a maximum time length and element count for the replay buffer.
[<JSEmitInline("{2}.replay({0}, {1})")>]
let replayBufferWindowImpl (bufferSize : Int32) (window : float) (source : IObservable<'Source>) = 
    Observable.Replay( source, bufferSize, TimeSpan.FromMilliseconds window )  

let replayBufferWindow (bufferSize : Int32) (window : TimeSpan) (source : IObservable<'Source>) = replayBufferWindowImpl bufferSize window.TotalMilliseconds source

//let replayBufferWindow  ( bufferSize:int )( window:TimeSpan )( source:IObservable<'Source>) : Subjects.IConnectableObservable<'Source> =
//        Observable.Replay( source )  
//
//
//
/// Returns an observable sequence that is the result of apply a map to a connectable observable sequence that
/// shares a single subscription to the underlying sequence replaying notifications subject to
/// a maximum element count for the replay buffer.                              
[<JSEmitInline("{2}.replay({0}, {1})")>]
let replayMapBuffer ( map ) ( bufferSize:int )( source:IObservable<'Source>) : IObservable<'Result> =
        Observable.Replay( source, Func<IObservable<'Source>,IObservable<'Result>>map, bufferSize )  


/// Returns an observable sequence that is the result of apply a map to a connectable observable sequence that
/// shares a single subscription to the underlying sequence replaying notifications subject to
/// a maximum time length.
//let replayMapWindow  ( map)( window:TimeSpan )( source:IObservable<'Source>) : IObservable<'Result> =
//        Observable.Replay( source,Func<IObservable<'Source>,IObservable<'Result>>  map, window )  
//
//
/// Returns an observable sequence that is the result of apply a map to a connectable observable sequence that
/// shares a single subscription to the underlying sequence replaying notifications subject to
/// a maximum time length and element count for the replay buffer.
[<JSEmitInline("{3}.replay({0}, {1}, {2})")>]
let replayMapBufferWindowImpl (map : IObservable<'Source> -> IObservable<'Result>) (bufferSize : Int32) (window : float) (source : IObservable<'Source>) =
    Observable.Replay( source, Func<IObservable<'Source>, IObservable<'Result>> map, bufferSize, TimeSpan.FromMilliseconds window )  

let replayMapBufferWindow (map : IObservable<'Source> -> IObservable<'Result>) (bufferSize : Int32) (window : TimeSpan) (source : IObservable<'Source>) = replayMapBufferWindowImpl map bufferSize window.TotalMilliseconds source

//let replayMapBufferWindow  ( map )( bufferSize:int ) ( window:TimeSpan ) ( source:IObservable<'Source>): IObservable<'Result> =
//    Observable.Replay( source, Func<IObservable<'Source>, IObservable<'Result>> map, bufferSize, window )  
//
//
/// Repeats the source observable sequence until it successfully terminates.
[<JSEmitInline("{0}.retry()")>]
let retry ( source:IObservable<'Source>) : IObservable<'Source> =
    Observable.Retry( source )


/// Repeats the source observable sequence the specified number of times or until it successfully terminates.
[<JSEmitInline("{1}.retry({0})")>]
let retryCount (count:int) ( source:IObservable<'Source>) : IObservable<'Source> =
    Observable.Retry( source, count )





[<JSEmitInline("Rx.Observable.return({0})")>]
let result x : IObservable<_> =
    Observable.Return x


    
/// Samples the observable at the given interval
[<JSEmitInline("{1}.sample({0})")>]
let sampleImpl (interval : float) (source : IObservable<'a>) =
    Observable.Sample(source, TimeSpan.FromMilliseconds interval)

let sample (interval : TimeSpan) (source : IObservable<'a>) = sampleImpl interval.TotalMilliseconds source

//let sample (interval: TimeSpan) source =
//    Observable.Sample(source, interval)
//
//
/// Samples the source observable sequence using a samper observable sequence producing sampling ticks.
/// Upon each sampling tick, the latest element (if any) in the source sequence during the 
/// last sampling interval is sent to the resulting sequence.
[<JSEmitInline("{1}.sample({0})")>]
let sampleWith   (sampler:IObservable<'Sample>) (source:IObservable<'Source>) : IObservable<'Source> =
    Observable.Sample( source, sampler )


/// Applies an accumulator function over an observable sequence and returns each intermediate result.
[<JSEmitInline("{1}.scan({0})")>]
let scan (accumulator:'a->'a->'a)  source =
    Observable.Scan(source, Func<'a,'a,'a> accumulator  )


/// Applies an accumulator function over an observable sequence and returns each intermediate result. 
/// The specified init value is used as the initial accumulator value.
[<JSEmitInline("{2}.scan({0}, {1})")>]
let scanInit (init:'TAccumulate) (accumulator) (source:IObservable<'Source>) : IObservable<'TAccumulate> =
    Observable.Scan( source, init, Func<'TAccumulate,'Source,'TAccumulate> accumulator )


/// If the condition evaluates true, select the "thenSource" sequence. Otherwise, return an empty sequence.
//let selectIf condition thenSource =
//    Observable.If( Func<bool> condition, thenSource )
//
//
/// If the condition evaluates true, select the "thenSource" sequence. Otherwise, select the else source 
//let selectIfElse condition ( elseSource : IObservable<'Result>) 
//                           ( thenSource : IObservable<'Result>) =
//    Observable.If( Func<bool> condition, thenSource, elseSource )
//
//
///  Returns an observable sequence that contains a single element.
[<JSEmitInline("Rx.Observable.return({0})")>]
let single ( value:'Result) : IObservable<'Result> =
   Observable.Return(value)


/// Bypasses a specified number of elements in an observable sequence and then returns the remaining elements.
[<JSEmitInline("{1}.skip({0})")>]
let skip (count:int) (source:IObservable<'Source>)  : IObservable<'Source> =
    Observable.Skip(source , count)


/// Skips elements for the specified duration from the start of the observable source sequence.
//let skipSpan  (duration:TimeSpan ) (source:IObservable<'Source> ): IObservable<'Source> =
//    Observable.Skip(source, duration)
//
//
/// Bypasses a specified number of elements at the end of an observable sequence.
[<JSEmitInline("{1}.skip({0})")>]
let skipLast  (count:int ) ( source:IObservable<'Source> ): IObservable<'Source> = 
    Observable.SkipLast (source, count )


/// Skips elements for the specified duration from the end of the observable source sequence.
[<JSEmitInline("{1}.skipLastWithTime({0})")>]
let skipLastSpanImpl (duration : float) (source : IObservable<'Source>) =
    Observable.SkipLast ( source, TimeSpan.FromMilliseconds duration)

let skipLastSpan (duration : TimeSpan) (source : IObservable<'Source>) = skipLastSpanImpl duration.TotalMilliseconds source

//let skipLastSpan (duration:TimeSpan ) ( source:IObservable<'Source>) : IObservable<'Source> =
//    Observable.SkipLast ( source, duration)
//
//
//
/// Skips elements from the observable source sequence until the specified start time.
//let skipUntil (startTime:DateTimeOffset ) ( source:IObservable<'Source> )  : IObservable<'Source> =
//    Observable.SkipUntil(source, startTime )
//
//
/// Returns the elements from the source observable sequence only after the other observable sequence produces an element.
[<JSEmitInline("{1}.skipUntil({0})")>]
let skipUntilOther ( other:IObservable<'Other> )  ( source:IObservable<'Source> ): IObservable<'Source> =
    Observable.SkipUntil(source, other )



/// Bypasses elements in an observable sequence as long as a specified condition is true and then returns the remaining elements.
[<JSEmitInline("{1}.skip({0})")>]
let skipWhile ( predicate:'Source -> bool ) ( source:IObservable<'Source> ): IObservable<'Source> =
    Observable.SkipWhile ( source, Func<'Source,bool> predicate ) 


/// Bypasses elements in an observable sequence as long as a specified condition is true and then returns the remaining elements.
/// The element's index is used in the logic of the predicate functio
//let skipWhilei ( predicate:'Source -> int -> bool)( source:IObservable<'Source> ) : IObservable<'Source> =
//    Observable.SkipWhile ( source, Func<'Source,int,bool> predicate)
//
//
/// Prepends a sequence of values to an observable sequence.   
[<JSEmitInline("{1}.startWith({0})")>]
let startWith  (values: #seq<'T>)  (source: IObservable<'T>) : IObservable<'T> = 
    // TODO: re-evaluate wrapping the overload that takes a params array when params are supported by F#.
    Observable.StartWith( source, values )


/// Subscribes to the Observable with a next fuction.
[<JSEmit("var subscription = {1}.subscribe({0});
subscription.Dispose = subscription.dispose;
return subscription;")>]
let subscribe(onNext: 'T -> unit) (observable: IObservable<'T>) =
      observable.Subscribe(Action<_> onNext)


/// Subscribes to the Observable with a next and an error-function.
[<JSEmit("var subscription = {2}.subscribe({0}, {1});
subscription.Dispose = subscription.dispose;
return subscription;")>]
let subscribeWithError  ( onNext     : 'T   -> unit     ) 
                        ( onError    : exn  -> unit     ) 
                        ( observable : IObservable<'T>  ) =
    observable.Subscribe( Action<_> onNext, Action<exn> onError )

 
/// Subscribes to the Observable with a next and a completion callback.
[<JSEmit("var subscription = {2}.subscribe({0}, {1});
subscription.Dispose = subscription.dispose;
return subscription;")>]
let subscribeWithCompletion (onNext: 'T -> unit) (onCompleted: unit -> unit) (observable: IObservable<'T>) =
        observable.Subscribe(Action<_> onNext, Action onCompleted)


/// Subscribes to the observable with all three callbacks
[<JSEmit("var subscription = {3}.subscribe({0}, {1}, {2});
subscription.Dispose = subscription.dispose;
return subscription;")>]
let subscribeWithCallbacks onNext onError onCompleted (observable: IObservable<'T>) =
    observable.Subscribe(Observer.Create(Action<_> onNext, Action<_> onError, Action onCompleted))


/// Subscribes to the observable with the given observer
[<JSEmit("var subscription = {1}.subscribe({0});
subscription.Dispose = subscription.dispose;
return subscription;")>]
let subscribeObserver observer (observable: IObservable<'T>) =
    observable.Subscribe observer


/// Wraps the source sequence in order to run its subscription and unsubscription logic 
/// on the specified scheduler. This operation is not commonly used;  This only performs 
/// the side-effects of subscription and unsubscription on the specified scheduler.
///  In order to invoke observer callbacks on a scheduler, use 'observeOn'
//let subscribeOn (context:Threading.SynchronizationContext) (source:IObservable<'Source>) : IObservable<'Source> =
//    Observable.SubscribeOn( source, context )
//
//
/// Transforms an observable sequence of observable sequences into an 
/// observable sequence producing values only from the most recent 
/// observable sequence.Each time a new inner observable sequnce is recieved,
/// unsubscribe from the previous inner sequence
[<JSEmitInline("{0}.switch()")>]
let switch (sources:IObservable<IObservable<'Source>>) : IObservable<'Source>= 
    Observable.Switch(sources)


/// Transforms an observable sequence of tasks into an observable sequence 
/// producing values only from the most recent observable sequence.
/// Each time a new task is received, the previous task's result is ignored.
//let switchTask ( sources:IObservable<Threading.Tasks.Task<'Source>>) : IObservable<'Source> =
//    Observable.Switch( sources )
//
//
//
/// Synchronizes the observable sequence so that notifications cannot be delivered concurrently
/// this voerload is useful to "fix" and observable sequence that exhibits concurrent 
/// callbacks on individual observers, which is invalid behavior for the query processor
//let synchronize  source : IObservable<'Source>= 
//    Observable.Synchronize( source )
//
//
/// Synchronizes the observable sequence such that observer notifications 
/// cannot be delivered concurrently, using the specified gate object.This 
/// overload is useful when writing n-ary query operators, in order to prevent 
/// concurrent callbacks from different sources by synchronizing on a common gate object.
//let synchronizeGate (gate:obj)  (source:IObservable<'Source>): IObservable<'Source> =
//    Observable.Synchronize( source, gate )
//
//
/// Takes n elements (from the beginning of an observable sequence? )
[<JSEmitInline("{1}.take({0})")>]
let take (n: int) source : IObservable<'Source> = 
    Observable.Take(source, n)    


/// Returns a specified number of contiguous elements from the end of an obserable sequence
[<JSEmitInline("{1}.take({0})")>]
let takeLast ( count:int ) source = 
    Observable.TakeLast(source, count)


/// Returns elements within the specified duration from the end of the observable source sequence.
[<JSEmitInline("{1}.takeLastWithTime({0})")>]
let takeLastSpanImpl (duration : float) (source : IObservable<'Source>) = 
    Observable.TakeLast( source, TimeSpan.FromMilliseconds duration)

let takeLastSpan (duration : TimeSpan) (source : IObservable<'Source>) = takeLastSpanImpl duration.TotalMilliseconds source

//let takeLastSpan ( duration:TimeSpan ) ( source:IObservable<'Source> ): IObservable<'Source> =
//    Observable.TakeLast( source, duration )
//
//
/// Returns a list with the elements within the specified duration from the end of the observable source sequence.
[<JSEmitInline("{1}.takeLastBufferWithTime({0})")>]
let takeLastBufferImpl (duration : float) (source : IObservable<'Source>) =
    Observable.TakeLastBuffer( source, TimeSpan.FromMilliseconds duration )

let takeLastBuffer (duration : TimeSpan) (source : IObservable<'Source>) = takeLastBufferImpl duration.TotalMilliseconds source

//let takeLastBuffer ( duration:TimeSpan )( source:IObservable<'Source> ): IObservable<Collections.Generic.IList<'Source>> =
//    Observable.TakeLastBuffer( source, duration )
//
//
/// Returns a list with the specified number of contiguous elements from the end of an observable sequence.
[<JSEmitInline("{1}.takeLastBuffer({0})")>]
let takeLastBufferCount ( count:int )( source:IObservable<'Source> ): IObservable<Collections.Generic.IList<'Source>> =
    Observable.TakeLastBuffer( source, count )


/// Returns the elements from the source observable sequence until the other produces and element
[<JSEmitInline("{1}.takeUntil({0})")>]
let takeUntilOther<'Other,'Source> other source =
    Observable.TakeUntil<'Source,'Other>(source , other )

//
/// Returns the elements from the source observable until the specified time
//let takeUntilTime<'Source> (endtime:DateTimeOffset) source =
//    Observable.TakeUntil<'Source>(source , endtime )
//
//
/// Returns elements from an observable sequence as long as a specified condition is true.
[<JSEmitInline("{1}.take({0})")>]
let takeWhile  (predicate) ( source:IObservable<'Source>): IObservable<'Source> =
    Observable.TakeWhile( source, Func<'Source,bool>predicate )


/// Returns elements from an observable sequence as long as a specified condition is true.
/// The element's index is used in the logic of the predicate functi
//let takeWhilei  ( predicate) (source:IObservable<'Source>) : IObservable<'Source> =
//    Observable.TakeWhile( source, Func<'Source,int,bool> predicate )
//
//
/// Ignores elements from an observable sequence which are followed by another element within a specified relative time duration.
[<JSEmitInline("{1}.throttle({0})")>]
let throttleImpl (dueTime : float) (source : IObservable<'Source>) =
    Observable.Throttle( source, TimeSpan.FromMilliseconds dueTime )

let throttle (dueTime : TimeSpan) (source : IObservable<'Source>) = throttleImpl dueTime.TotalMilliseconds source

//let throttle  (dueTime:TimeSpan) (source:IObservable<'Source>): IObservable<'Source> =
//    Observable.Throttle( source, dueTime )
//
//
/// Ignores elements from an observable sequence which are followed by another value within a computed throttle duration
[<JSEmitInline("{1}.throttleWithSelector({0})")>]
let throttleComputed ( source:IObservable<'Source>) (throttleDurationSelector) : IObservable<'Source> =
    Observable.Throttle( source, Func<'Source,IObservable<'Throttle>> throttleDurationSelector )


/// Returns an observable sequence that terminates with an exception.
[<JSEmitInline("Rx.Observable.throw({0})")>]
let throw ( except:exn ) : IObservable<'Result> =
    Observable.Throw( except )


/// matches when the observable sequence has an available element and 
/// applies the map
//let thenMap map source = 
//    Observable.Then( source, Func<'Source,'Result> map )
//
//
/// Records the time interval between consecutive elements in an observable sequence.
//let timeInterval ( source:IObservable<'Source>) : IObservable<TimeInterval<'Source>> =
//    Observable.TimeInterval( source )
//
//
/// Applies a timeout policy to the observable sequence based on an absolute time.
/// If the sequence doesn't terminate before the specified absolute due time, a TimeoutException is propagated to the observer.
//let timeout ( timeout:System.DateTimeOffset ) ( source:IObservable<'Source>) =
//    Observable.Timeout( source, timeout)
//
//
/// Applies a timeout policy to the observable sequence based on an absolute time.
/// If the sequence doesn't terminate before the specified absolute due time, the other 
/// observable sequence is used to produce future messages from that point on.
//let timeoutOther ( timeout:System.DateTimeOffset ) ( other:IObservable<'Source>) ( source:IObservable<'Source>) =
//    Observable.Timeout( source, timeout, other)
//
//
/// Applies a timeout policy for each element in the observable sequence.
/// If the next element isn't received within the specified timeout duration starting from its
/// predecessor, a TimeoutException is propagated to the observer.
//let timeoutSpan ( timeout:TimeSpan ) ( source:IObservable<'Source> ) =
//    Observable.Timeout( source, timeout)
//    
//
/// Applies a timeout policy for each element in the observable sequence.
/// If the next element isn't received within the specified timeout duration starting from 
/// its predecessor, the other observable sequence is used to produce future messages from that point on.
//let timeoutSpanOther( timeout:TimeSpan ) ( other:IObservable<'Source> ) ( source:IObservable<'Source> ) =
//    Observable.Timeout( source, timeout, other)
//
//
/// Applies a timeout policy to the observable sequence based on a timeout duration computed for each element.
/// If the next element isn't received within the computed duration starting from its predecessor,
/// a TimeoutException is propagated to the observer.
[<JSEmitInline("{1}.timeoutwithselector({0})")>]
let timeoutDuration ( durationSelector )( source:IObservable<'Source> ) : IObservable<'Source> =
    Observable.Timeout( source, Func<'Source,IObservable<'Timeout>> durationSelector   )


/// Applies a timeout policy to the observable sequence based on an initial timeout duration 
/// for the first element, and a timeout duration computed for each subsequent element.
/// If the next element isn't received within the computed duration starting from its predecessor, 
/// a TimeoutException is propagated to the observer.
//let timeout2Duration ( timeout:IObservable<'Timeout> ) 
//                     ( durationSelector              ) 
//                     ( source:IObservable<'Source>   ) =
//    Observable.Timeout( source, Func<'Source, IObservable<'Timeout>> durationSelector)
//
//
//
/// Applies a timeout policy to the observable sequence based on an initial timeout duration for the first
/// element, and a timeout duration computed for each subsequent element.
/// If the next element isn't received within the computed duration starting from its predecessor, 
/// the other observable sequence is used to produce future messages from that point on.
//let timeout2DurationOther   ( timeout: IObservable<'Timeout>) 
//                            ( durationSelector              )
//                            ( other  : IObservable<'Source> ) 
//                            ( source : IObservable<'Source> ) =
//    Observable.Timeout( source, timeout, Func<'Source, IObservable<'Timeout>> durationSelector, other)
//
//
//    #endregion
//
//
/// Returns an observable sequence that produces a single value at the specified absolute due time.
//let timer ( dueTime:DateTimeOffset ) : IObservable<int64> =
//    Observable.Timer( dueTime )
//
//
/// Returns an observable sequence that periodically produces a value starting at the specified initial absolute due time.
//let timerPeriod ( dueTime:DateTimeOffset) ( period:TimeSpan ) : IObservable<int64> =
//    Observable.Timer( dueTime, period)
//
//
/// Returns an observable sequence that produces a single value after the specified relative due time has elapsed.
//let timerSpan ( dueTime:TimeSpan ) : IObservable<int64> =   
//    Observable.Timer( dueTime )
//
//
/// Returns an observable sequence that periodically produces a value after the specified
/// initial relative due time has elapsed.
//let timerSpanPeriod ( dueTime:TimeSpan, period:TimeSpan ) : IObservable<int64> =
//    Observable.Timer( dueTime, period)
//
//
/// Timestamps each element in an observable sequence using the local system clock.
//let timestamp ( source:IObservable<'Source> ) : IObservable<Timestamped<'Source>> =
//    Observable.Timestamp( source )
//
//
/// Converts an observable into a seq
//let toEnumerable (source: IObservable<'T>) = Observable.ToEnumerable(source)
/// Creates an array from an observable sequence
//
//
/// Creates an array from an observable sequence.
//let toArray  source = 
//    Observable.ToArray(source)
//
//
/// Creates an observable sequence according to a specified key selector function
//let toDictionary keySelector source = 
//    Observable.ToDictionary(source, keySelector)
//
//
/// Creates an observable sequence according to a specified key selector function
/// and an a comparer
//let toDictionaryComparer (keySelector:'Source->'Key) (comparer:'Key) (source:'Source) =
//    Observable.ToDictionary( source, keySelector, comparer )
//
//
/// Creates an observable sequence according to a specified key selector function
//let toDictionaryElements (keySelector:'Source->'Key )(elementSelector:'Source->'Elm) (source:'Source) =
//    Observable.ToDictionary(source, keySelector, elementSelector)    
//
//
/// Creates an observable sequence according to a specified key selector function
//let toDictionaryCompareElements ( keySelector    : 'Source -> 'Key  )
//                                ( elementSelector: 'Source ->' Elm  ) 
//                                ( comparer:'Key ) ( source:'Source  ) =
//    Observable.ToDictionary(    source                              , 
//                                Func<'Source,'Key> keySelector      , 
//                                Func<'Source,'Elm> elementSelector  , 
//                                comparer                            ) 
//
//
/// Exposes and observable sequence as an object with an Action based .NET event
//let toEvent (source:IObservable<unit>) = 
//    Observable.ToEvent(source)
//
//
/// Exposes an observable sequence as an object with an Action<'Source> based .NET event.
//let toEventType ( source:IObservable<'Source> ) : IEventSource<'Source> =
//    Observable.ToEvent(source)
//
//
/// Creates a list from an observable sequence
//let toList source = 
//    Observable.ToList(source)
//
//
/// Creates a lookup from an observable sequence according to a specified key selector function.
//let toLookup ( keySelector )( source:IObservable<'Source> ) : IObservable<Linq.ILookup<'Key,'Source>> =
//   Observable.ToLookup( source, Func<'Source,'Key> keySelector )
//
//
/// Creates a lookup from an observable sequence according to a specified key selector function, and a comparer.
//let toLookupCompare ( keySelector ) ( comparer:IEqualityComparer<'Key> )( source:IObservable<'Source> ) : IObservable<Linq.ILookup<'Key,'Source>> =
//   Observable.ToLookup( source,Func<'Source,'Key> keySelector, comparer)
//
//
/// Creates a lookup from an observable sequence according to a specified key selector function, and an element selector function.
//let toLookupElement ( keySelector ) ( elementSelector ) ( comparer:IEqualityComparer<'Key>)( source:IObservable<'Source> ) : IObservable<Linq.ILookup<'Key,'Element>>=
//   Observable.ToLookup( source, Func<'Source,'Key> keySelector, Func<'Source,'Element> elementSelector, comparer )
//
//
/// Creates a lookup from an observable sequence according to a specified key selector function, and an element selector function.
//let toLookupCompareElement ( keySelector ) ( elementSelector )( source:IObservable<'Source> ) : IObservable<Linq.ILookup<'Key,'Element>> =
//   Observable.ToLookup( source,Func<'Source,'Key>  keySelector, Func<'Source,'Element> elementSelector )
//
//
/// Converts a seq into an observable
[<JSEmitInline("Rx.Observable.fromArray({0})")>]
let toObservableImpl (source : array<'T>) = source.ToObservable()

let toObservable (source : IEnumerable<'T>) = toObservableImpl (Seq.toArray source)

//let toObservable ( source: seq<'T> ) = Observable.ToObservable(source)
//
//
/// Constructs an observable sequence that depends on a resource object, whose 
/// lifetime is tied to the resulting observable sequence's lifetime.
//let using ( resourceFactory: unit ->'TResource ) (observableFactory: 'TResource -> IObservable<'Result> ) : IObservable<'Result> =
//    Observable.Using ( Func<_> resourceFactory, Func<_,_> observableFactory )
//
//
/// waits for the observable sequence to complete and returns the last
/// element of the sequence. If the sequence terminates with OnError
/// notification, the exception is thrown
//let wait  source = 
//    Observable.Wait( source )
//
//
/// Repeats the given function as long as the specified condition holds
/// where the condition is evaluated before each repeated source is 
/// subscribed to
//let whileLoop condition source = 
//    Observable.While( Func<bool> condition, source ) 
//    
//
/// Projects each element of an observable sequence into consecutive non-overlapping windows.
/// windowClosingSelector - A function invoked to define the boundaries of the produced windows. 
/// A new window is started when the previous one is closed
[<JSEmitInline("{1}.window({0})")>]
let window ( windowClosingSelector ) ( source:IObservable<'Source> ) : IObservable<IObservable<'Source>> =
    Observable.Window( source, Func<IObservable<'WindowClosing>> windowClosingSelector)


/// Projects each element of an observable sequence into consecutive non-overlapping windows 
/// which are produced based on timing information.
[<JSEmitInline("{1}.windowWithTime({0})")>]
let windowTimeSpanImpl (timeSpan : float) (source : IObservable<'Source>) =
    Observable.Window( source, TimeSpan.FromMilliseconds timeSpan )

let windowTimeSpan (timeSpan : TimeSpan) (source : IObservable<'Source>) = windowTimeSpanImpl timeSpan.TotalMilliseconds source

//let windowTimeSpan ( timeSpan:TimeSpan )( source:IObservable<'Source> ) : IObservable<IObservable<'Source>> =
//    Observable.Window( source, timeSpan )
//
//
/// Projects each element of an observable sequence into zero or more windows.
/// windowOpenings - Observable sequence whose elements denote the creation of new windows.
/// windowClosingSelector - A function invoked to define the closing of each produced window.
[<JSEmitInline("{2}.window({0}, {1})")>]
let windowOpenClose ( windowOpenings        : IObservable<'WinOpen>             )
                    ( windowClosingSelector : 'WinOpen->IObservable<'WinClose>  )
                    ( source                : IObservable<'Source>              ) : IObservable<IObservable<'Source>> =
    Observable.Window(source, windowOpenings, Func<_,_> windowClosingSelector)


/// Projects each element of an observable sequence into consecutive non-overlapping windows.
/// windowBoundaries - Sequence of window boundary markers. The current window is closed and a new window is opened upon receiving a boundary marker.
[<JSEmitInline("{2}.windowWithTime({0}, {1})")>]
let windowTimeShiftImpl (timeSpan : float) (timeShift : float) (source : IObservable<'Source>) = 
    Observable.Window( source, TimeSpan.FromMilliseconds timeSpan, TimeSpan.FromMilliseconds timeShift )

let windowTimeShift (timeSpan : TimeSpan) (timeShift : TimeSpan) (source : IObservable<'Source>) = windowTimeShiftImpl timeSpan.TotalMilliseconds timeShift.TotalMilliseconds source

//let windowTimeShift ( timeSpan:TimeSpan )( timeShift:TimeSpan )( source:IObservable<'Source> ) : IObservable<IObservable<'Source>> =
//    Observable.Window( source, timeSpan, timeShift )
//
//
//
/// Projects each element of an observable sequence into consecutive non-overlapping windows
/// windowBoundaries - Sequence of window boundary markers. The current window is closed 
/// and a new window is opened upon receiving a boundary marker
//let windowBounded    ( windowBoundaries:IObservable<'WindowBoundary> )( source:IObservable<'Source> ) : IObservable<IObservable<'Source>> =
//    Observable.Window( source, windowBoundaries )
//
//
/// Projects each element of an observable sequence into zero or more windows which are produced based on element count information
[<JSEmitInline("{2}.windowWithCount({0}, {1})")>]
let windowCountSkip ( count:int )( skip:int ) ( source:IObservable<'Source> ): IObservable<IObservable<'Source>> =
    Observable.Window( source, count, skip )


/// Projects each element of an observable sequence into consecutive non-overlapping windows 
/// which are produced based on element count information.
[<JSEmitInline("{1}.windowWithCount({0})")>]
let windowCount ( count:int )( source:IObservable<'Source> ) : IObservable<IObservable<'Source>> =
    Observable.Window( source, count )


/// Projects each element of an observable sequence into a window that is completed when either it's full or 
/// a given amount of time has elapsed.
/// A useful real-world analogy of this overload is the behavior of a ferry leaving the dock when all seats are 
/// taken, or at the scheduled time of departure, whichever event occurs first
[<JSEmitInline("{2}.windowWithTimeOrCount({0}, {1})")>]
let windowTimeCountImpl (timeSpan : float) (count : Int32) (source : IObservable<'Source>) = 
    Observable.Window( source, TimeSpan.FromMilliseconds timeSpan, count )

let windowTimeCount (timeSpan : TimeSpan) (count : Int32) (source : IObservable<'Source>) = windowTimeCountImpl timeSpan.TotalMilliseconds count source

//let windowTimeCount ( timeSpan:TimeSpan ) (count:int) ( source:IObservable<'Source> ): IObservable<IObservable<'Source>> =
//    Observable.Window( source, timeSpan, count )
//
//
/// Merges two observable sequences into one observable sequence by combining their elements in a pairwise fashion.
//let zip ( first:IObservable<'Source1>)( second:IObservable<'Source2>) ( resultSelector:'Source1 -> 'Source2 -> 'Result) : IObservable<'Result> =
//    Observable.Zip( first, second, Func<'Source1,'Source2,'Result> resultSelector)
//
//
/// Merges the specified observable sequences into one observable sequence by emitting a
///  list with the elements of the observable sequences at corresponding indexes.
[<JSEmitInline("Rx.Observable.zip({0})")>]
let zipSeqImpl (sources : array<IObservable<'Source>>) = Observable.Zip( sources )

let zipSeq (sources : IEnumerable<IObservable<'Source>>) = zipSeqImpl (Seq.toArray sources)

//let zipSeq ( sources:seq<IObservable<'Source>>) : IObservable<IList<'Source>> = 
//    Observable.Zip( sources )
//
//
/// Merges the specified observable sequences into one observable sequence by emitting 
/// a list with the elements of the observable sequences at corresponding indexe
[<JSEmitInline("Rx.Observable.zip({0})")>]
let zipArray ( sources:IObservable<'Source> []) : IObservable<IList<'Source>> =
    Observable.Zip( sources )
 

/// Merges the specified observable sequences into one observable sequence by using 
/// the selector function whenever all of the observable sequences have produced an 
/// element at a corresponding index.
//let zipSeqMap ( resultSelector: IList<'S> ->'R) ( sources: seq<IObservable<'S>>)  : IObservable<'R> =
//    Observable.Zip( sources, Func<IList<'S>,'R> resultSelector)
// 
//
// 
/// Merges an observable sequence and an enumerable sequence into one 
/// observable sequence by using the selector function.
//let zipWithSeq ( resultSelector: 'Source1 -> 'Source2 -> 'Result   )
//               ( second        : seq<'Source2>                       )
//               ( first         : IObservable<'Source1>               ) : IObservable<'Result> =
//    Observable.Zip(first, second, Func<_,_,_> resultSelector )
// 
