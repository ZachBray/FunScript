[<FunJS.JS>]
module FunJS.Tests.Seqs

open Xunit
open FsUnit.Xunit

let sumFirstTwo zs =
   let first = Seq.head zs
   let second = Seq.skip 1 zs |> Seq.head
   first + second

[<Fact>]
let ``Seq.length works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         float (Seq.length xs)
      @@>

[<Fact>]
let ``Seq.delay works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = Seq.delay (fun () -> xs :> _ seq)
         ys |> Seq.head
      @@>

[<Fact>]
let ``Seq.unfold works``() =
   check  
      <@@ 
         1 |> Seq.unfold (fun x ->
            if x <= 5 then Some(x, x + 1)
            else None)
         |> Seq.length
         |> float
      @@>

[<Fact>]
let ``Seq.empty works``() =
   check  
      <@@ 
         let xs = Seq.empty<int>
         xs.GetEnumerator().MoveNext()
      @@>

[<Fact>]
let ``Seq.append works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [0.]
         let zs = Seq.append ys xs
         sumFirstTwo zs
      @@>

[<Fact>]
let ``Seq.average works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         Seq.average xs
      @@>

[<Fact>]
let ``Seq.averageBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         Seq.averageBy ((*) 2.) xs
      @@>

[<Fact>]
let ``Seq.choose works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let zs = xs |> Seq.choose (fun x ->
            if x > 2. then Some x
            else None) 
         sumFirstTwo zs
      @@>

[<Fact>]
let ``Seq.concat works``() =
   check  
      <@@ 
         let xs = [[1.]; [2.]; [3.]; [4.]]
         let ys = xs |> Seq.concat
         sumFirstTwo ys
      @@>

[<Fact>]
let ``Seq.collect works``() =
   check  
      <@@ 
         let xs = [[1.]; [2.]; [3.]; [4.]]
         let ys = xs |> Seq.collect id
         sumFirstTwo ys
      @@>

[<Fact>]
let ``Seq.exists works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.exists (fun x -> x = 2.)
      @@>

[<Fact>]
let ``Seq.exists2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         Seq.exists2 (fun x y -> x * y = 16.) xs ys
      @@>

[<Fact>]
let ``Seq.filter works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = xs |> Seq.filter (fun x -> x > 5.)
         ys |> Seq.isEmpty
      @@>

[<Fact>]
let ``Seq.find works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.find ((=) 2.)
      @@>

[<Fact>]
let ``Seq.findIndex works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.findIndex ((=) 2.)
         |> float
      @@>

[<Fact>]
let ``Seq.fold works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = xs |> Seq.fold (+) 0.
         total
      @@>

[<Fact>]
let ``Seq.forall works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         Seq.forall (fun x -> x < 5.) xs
      @@>

[<Fact>]
let ``Seq.forall2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         Seq.forall2 (=) xs ys
      @@>

[<Fact>]
let ``Seq.head works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         Seq.head xs
      @@>

[<Fact>]
let ``Seq.init works``() =
   check  
      <@@ 
         let xs = Seq.init 4 float
         sumFirstTwo xs
      @@>

[<Fact>]
let ``Seq.isEmpty works``() =
   check  
      <@@ 
         let xs = [1]
         Seq.isEmpty xs
      @@>

[<Fact>]
let ``Seq.iter works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         xs |> Seq.iter (fun x ->
            total := !total + x
         )
         !total
      @@>

[<Fact>]
let ``Seq.iter2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         Seq.iter2 (fun x y ->
            total := !total + x + y
         ) xs xs
         !total
      @@>

[<Fact>]
let ``Seq.iteri works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         xs |> Seq.iteri (fun i x ->
            total := !total + (float i) * x
         )
         !total
      @@>

[<Fact>]
let ``Seq.map works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = xs |> Seq.map ((*) 2.)
         ys |> Seq.head
      @@>

[<Fact>]
let ``Seq.map2 works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = [2.]
         let zs = Seq.map2 (*) xs ys
         zs |> Seq.head
      @@>

[<Fact>]
let ``Seq.mapi works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = xs |> Seq.mapi (fun i x -> float i + x)
         ys |> Seq.head
      @@>

[<Fact>]
let ``Seq.max works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.max
      @@>

[<Fact>]
let ``Seq.maxBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.maxBy (fun x -> -x)
      @@>

[<Fact>]
let ``Seq.min works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.min
      @@>

[<Fact>]
let ``Seq.minBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.minBy (fun x -> -x)
      @@>

[<Fact>]
let ``Seq.nth works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         Seq.nth 1 xs
      @@>

[<Fact>]
let ``Seq.ofArray works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = Seq.ofArray xs
         ys |> Seq.head
      @@>

[<Fact>]
let ``Seq.ofList works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = Seq.ofList xs
         ys |> Seq.head
      @@>

[<Fact>]
let ``Seq.pick works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.pick (fun x ->
            match x with
            | 2. -> Some x
            | _ -> None)
      @@>

[<Fact>]
let ``Seq.reduce works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.reduce (+)
      @@>

[<Fact>]
let ``Seq.scan works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = xs |> Seq.scan (+) 0.
         sumFirstTwo ys
      @@>

[<Fact(Skip="Jint cannot interpret properly Array.sort()")>]
let ``Seq.sort works``() =
   check  
      <@@ 
         let xs = [3.; 4.; 1.; 2.]
         let ys = xs |> Seq.sort
         sumFirstTwo ys
      @@>

[<Fact(Skip="Need to think about IComparable")>]
let ``Seq.sortBy works``() =
   check  
      <@@ 
         let xs = [3.; 4.; 1.; 2.]
         let ys = xs |> Seq.sortBy (fun x -> -x)
         sumFirstTwo ys
      @@>

[<Fact>]
let ``Seq.sum works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.sum
      @@>

[<Fact>]
let ``Seq.sumBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.sumBy ((*) 2.)
      @@>

[<Fact>]
let ``Seq.skip works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = xs |> Seq.skip 1
         ys |> Seq.head
      @@>

[<Fact>]
let ``Seq.toArray works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = xs |> Seq.toArray
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``Seq.toList works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = xs |> Seq.toList
         ys.Head + ys.Tail.Head
      @@>

[<Fact>]
let ``Seq.tryFind works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> Seq.tryFind ((=) 1.)
         ys.IsSome
      @@>

[<Fact>]
let ``Seq.tryFindIndex works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> Seq.tryFindIndex ((=) 2.)
         ys.Value |> float
      @@>

[<Fact>]
let ``Seq.tryPick works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let r = xs |> Seq.tryPick (fun x ->
            match x with
            | 2. -> Some x
            | _ -> None)
         match r with
         | Some x -> x
         | None -> 0.
      @@>

[<Fact>]
let ``Seq.zip works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = [1.; 2.; 3.]
         let zs = Seq.zip xs ys
         let x, y = zs |> Seq.head
         x + y
      @@>

[<Fact>]
let ``Seq.zip3 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = [1.; 2.; 3.]
         let zs = [1.; 2.; 3.]
         let ks = Seq.zip3 xs ys zs
         let x, y, z = ks |> Seq.head
         x + y + z
      @@>

let ``Seq.cache works``() =
   check
      <@@
         let count = ref 0
         let xs = 
            1 |> Seq.unfold(fun i -> 
               count := !count + 1
               if i <= 10 then Some(i, i+1)
               else None)
         xs |> Seq.length |> ignore
         xs |> Seq.length |> ignore
         !count
      @@>

[<Fact>]
let ``Seq.cast works``() =
   check  
      <@@ 
         let xs = [1; 2; 3; 4]
         let ys = Seq.cast<int> xs
         ys |> Seq.head |> float
      @@>

[<Fact>]
let ``Seq.compareWith works``() =
   check  
      <@@ 
         let xs = [1; 2; 3; 4]
         let ys = [1; 2; 3; 4]
         let diff = Seq.compareWith (fun x y -> x - y) xs ys 
         float diff
      @@>

[<Fact(Skip="Need to figure out comparing")>]
let ``Seq.countBy works``() =
   check  
      <@@ 
         let xs = [1; 2; 3; 4]
         let ys = xs |> Seq.countBy (fun x -> x % 2)
         ys |> Seq.length |> float
      @@>

[<Fact(Skip="Need to figure out comparing")>]
let ``Seq.distinct works``() =
   check  
      <@@ 
         let xs = [1; 1; 1; 2; 2; 3; 3]
         let ys = xs |> Seq.distinct
         ys |> Seq.length |> float
      @@>

[<Fact(Skip="Need to figure out comparing")>]
let ``Seq.distinctBy works``() =
   check  
      <@@ 
         let xs = [1; 1; 1; 2; 2; 3; 3]
         let ys = xs |> Seq.distinctBy (fun x -> x % 2)
         ys |> Seq.length |> float
      @@>

[<Fact>]
let ``Seq.exactlyOne works``() =
   check  
      <@@ 
         let xs = [1.]
         xs |> Seq.exactlyOne
      @@>

[<Fact(Skip="Need to figure out comparing")>]
let ``Seq.groupBy works``() =
   check  
      <@@ 
         let xs = [1; 2; 3; 4]
         let ys = xs |> Seq.groupBy (fun x -> x % 2)
         ys |> Seq.length |> float
      @@>

[<Fact>]
let ``Seq.initInfinite works``() =
   check  
      <@@ 
         Seq.initInfinite (fun i -> 2. * float i)
         |> Seq.take 10
         |> Seq.sum
      @@>

[<Fact>]
let ``Seq.last works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.last
      @@>

[<Fact>]
let ``Seq.pairwise works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.pairwise
         |> Seq.map (fun (x, y) -> x * y)
         |> Seq.sum
      @@>

[<Fact>]
let ``Seq.readonly works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.readonly
         |> Seq.head
      @@>

[<Fact>]
let ``Seq.singleton works``() =
   check  
      <@@ 
         let xs = Seq.singleton 1.
         xs |> Seq.head
      @@>

[<Fact>]
let ``Seq.skipWhile works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.skipWhile (fun i -> i <= 3.)
         |> Seq.head
      @@>

[<Fact>]
let ``Seq.take works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.take 2
         |> Seq.last
      @@>

[<Fact>]
let ``Seq.takeWhile works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.takeWhile (fun i -> i < 3.)
         |> Seq.last
      @@>

[<Fact>]
let ``Seq.truncate works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.truncate 2
         |> Seq.last
      @@>

[<Fact>]
let ``Seq.where works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.where (fun i -> i <= 3.)
         |> Seq.length 
         |> float
      @@>