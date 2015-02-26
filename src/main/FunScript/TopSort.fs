module FunScript.Sorting

open System.Collections.Generic

let TopSort(roots : seq<'T>, pred : 'T -> seq<'T>, nodeIdentity : IEqualityComparer<'T>) = 
    seq { 
        let visited = HashSet(nodeIdentity)
        
        let rec visit node = 
            seq { 
                if visited.Add(node) then 
                    for pN in pred node do
                        yield! visit pN
                    yield node
            }
        for node in roots do
            yield! visit node
    }