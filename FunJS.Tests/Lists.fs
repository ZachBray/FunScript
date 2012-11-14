module FunJS.Tests.Lists

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``List literals work``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         ()
      @@>

[<Fact>]
let ``List.Length works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.Length |> float
      @@>

[<Fact>]
let ``List.IsEmpty works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.IsEmpty
      @@>

[<Fact>]
let ``List.Head works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.Head
      @@>

[<Fact>]
let ``List.Tail works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.Tail.Head
      @@>

[<Fact>]
let ``List.Item works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.[3]
      @@>

[<Fact>]
let ``List cons works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = 0. :: xs
         let zs = List.Cons(0., xs)
         ys.Head + xs.Head
      @@>

[<Fact>]
let ``List.empty works``() =
   check  
      <@@ 
         let xs = 1. :: List.Empty
         let ys = 1. :: List.empty
         xs.Head + ys.Head
      @@>

[<Fact>]
let ``List.append works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [0.]
         let zs = List.append ys xs
         zs.Head + zs.Tail.Head
      @@>

[<Fact>]
let ``List.average works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.average xs
      @@>

[<Fact>]
let ``List.averageBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.averageBy ((*) 2.) xs
      @@>

[<Fact>]
let ``List.choose works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let result = xs |> List.choose (fun x ->
            if x > 2. then Some x
            else None) 
         result.Head + result.Tail.Head
      @@>

[<Fact>]
let ``List.collect works``() =
   check  
      <@@ 
         let xs = [[1.]; [2.]; [3.]; [4.]]
         let ys = xs |> List.collect id
         ys.Head  + ys.Tail.Head
      @@>

[<Fact(Skip="Need to think about IEnumerable")>]
let ``List.concat works``() =
   check  
      <@@ 
         let xs = [[1.]; [2.]; [3.]; [4.]]
         let ys = xs |> List.concat
         ys.Head  + ys.Tail.Head
      @@>

[<Fact>]
let ``List.exists works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> List.exists (fun x -> x = 2.)
      @@>

[<Fact>]
let ``List.exists2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         List.exists2 (fun x y -> x * y = 16.) xs ys
      @@>

[<Fact>]
let ``List.filter works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = xs |> List.filter (fun x -> x > 5.)
         ys.IsEmpty
      @@>

[<Fact>]
let ``List.find works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> List.find ((=) 2.)
      @@>

[<Fact>]
let ``List.findIndex works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> List.findIndex ((=) 2.)
         |> float
      @@>

[<Fact>]
let ``List.fold works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = xs |> List.fold (+) 0.
         total
      @@>

[<Fact>]
let ``List.fold2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         let total = List.fold2 (fun x y z -> x + y + z) 0. xs ys
         total
      @@>

[<Fact>]
let ``List.foldBack works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = List.foldBack (+) xs 0.
         total
      @@>

[<Fact>]
let ``List.foldBack2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         let total = List.foldBack2 (fun x y z -> x + y + z) xs ys 0.
         total
      @@>

[<Fact>]
let ``List.forall works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.forall (fun x -> x < 5.) xs
      @@>

[<Fact>]
let ``List.forall2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         List.forall2 (=) xs ys
      @@>

[<Fact>]
let ``List.head works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.head xs
      @@>

[<Fact>]
let ``List.init works``() =
   check  
      <@@ 
         let xs = List.init 4 float
         xs.Head + xs.Tail.Head
      @@>

[<Fact>]
let ``List.isEmpty works``() =
   check  
      <@@ 
         let xs = [1]
         List.isEmpty xs
      @@>

[<Fact>]
let ``List.iter works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         xs |> List.iter (fun x ->
            total := !total + x
         )
         !total
      @@>

[<Fact>]
let ``List.iter2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         List.iter2 (fun x y ->
            total := !total + x + y
         ) xs xs
         !total
      @@>

[<Fact>]
let ``List.iteri works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         xs |> List.iteri (fun i x ->
            total := !total + (float i) * x
         )
         !total
      @@>

[<Fact>]
let ``List.iteri2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         List.iteri2 (fun i x y ->
            total := !total + (float i) * x + (float i) * y
         ) xs xs
         !total
      @@>

[<Fact>]
let ``List.length works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.length xs 
         |> float
      @@>

[<Fact>]
let ``List.map works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = xs |> List.map ((*) 2.)
         ys.Head
      @@>

[<Fact>]
let ``List.map2 works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = [2.]
         let zs = List.map2 (*) xs ys
         zs.Head
      @@>

[<Fact>]
let ``List.map3 works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = [2.]
         let zs = [3.]
         let ks = List.map3 (fun x y z -> x * y * z) xs ys zs
         zs.Head
      @@>

[<Fact>]
let ``List.mapi works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = xs |> List.mapi (fun i x -> float i + x)
         ys.Head
      @@>

[<Fact>]
let ``List.mapi2 works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = [2.]
         let zs = List.mapi2 (fun i x y -> float i + x * y) xs ys
         zs.Head
      @@>

[<Fact>]
let ``List.max works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.max
      @@>

[<Fact>]
let ``List.maxBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.maxBy (fun x -> -x)
      @@>

[<Fact>]
let ``List.min works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.min
      @@>

[<Fact>]
let ``List.minBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.minBy (fun x -> -x)
      @@>

[<Fact>]
let ``List.nth works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         List.nth xs 1
      @@>

[<Fact>]
let ``List.ofArray works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = List.ofArray xs
         ys.Head
      @@>

[<Fact>]
let ``List.partition works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys, zs = xs |> List.partition (fun x -> x <= 1.)
         ys.Head - zs.Head
      @@>

[<Fact>]
let ``List.permute works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.permute (fun i -> i + 1 - 2 * (i % 2))
         ys.Head
      @@>

[<Fact>]
let ``List.pick works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.pick (fun x ->
            match x with
            | 2. -> Some x
            | _ -> None)
      @@>

[<Fact>]
let ``List.reduce works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.reduce (+)
      @@>

[<Fact>]
let ``List.reduceBack works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.reduceBack (+)
      @@>

[<Fact>]
let ``List.replicate works``() =
   check  
      <@@ 
         let xs = List.replicate 2 1.
         xs.Head + xs.Tail.Head
      @@>

[<Fact>]
let ``List.rev works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.rev
         ys.Head
      @@>

[<Fact>]
let ``List.scan works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = xs |> List.scan (+) 0.
         ys.Head + ys.Tail.Head
      @@>

[<Fact>]
let ``List.scanBack works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = List.scanBack (+) xs 0.
         ys.Head + ys.Tail.Head
      @@>


[<Fact(Skip="Jint cannot interpret properly Array.sort()")>]
let ``List.sort works``() =
   check  
      <@@ 
         let xs = [3.; 4.; 1.; 2.]
         let ys = xs |> List.sort
         ys.Head + ys.Tail.Head
      @@>

[<Fact(Skip="Need to think about IComparable")>]
let ``List.sortBy works``() =
   check  
      <@@ 
         let xs = [3.; 4.; 1.; 2.]
         let ys = xs |> List.sortBy (fun x -> -x)
         ys.Head + ys.Tail.Head
      @@>

[<Fact>]
let ``List.sortWith works``() =
   check  
      <@@ 
         let xs = [3.; 4.; 1.; 2.]
         let ys = xs |> List.sortWith (fun x y -> int(x - y))
         ys.Head + ys.Tail.Head
      @@>

[<Fact>]
let ``List.sum works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.sum
      @@>

[<Fact>]
let ``List.sumBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.sumBy ((*) 2.)
      @@>

[<Fact>]
let ``List.tail works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.tail
         ys.Head
      @@>

[<Fact>]
let ``List.toArray works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.toArray
         ys.[0] + ys.[1]
      @@>

[<Fact>]
let ``List.toSeq works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.toSeq
         ys.GetEnumerator().MoveNext()
      @@>

[<Fact>]
let ``List.tryFind works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.tryFind ((=) 1.)
         ys.IsSome
      @@>

[<Fact>]
let ``List.tryFindIndex works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.tryFindIndex ((=) 2.)
         ys.Value |> float
      @@>

[<Fact>]
let ``List.tryPick works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let r = xs |> List.tryPick (fun x ->
            match x with
            | 2. -> Some x
            | _ -> None)
         match r with
         | Some x -> x
         | None -> 0.
      @@>

[<Fact>]
let ``List.unzip works``() =
   check  
      <@@ 
         let xs = [1., 2.]
         let ys, zs = xs |> List.unzip
         ys.Head + zs.Head
      @@>

[<Fact>]
let ``List.unzip3 works``() =
   check  
      <@@ 
         let xs = [1., 2., 3.]
         let ys, zs, ks = xs |> List.unzip3
         ys.Head + zs.Head + ks.Head
      @@>

[<Fact>]
let ``List.zip works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = [1.; 2.; 3.]
         let zs = List.zip xs ys
         let x, y = zs.Head
         x + y
      @@>

[<Fact>]
let ``List.zip3 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = [1.; 2.; 3.]
         let zs = [1.; 2.; 3.]
         let ks = List.zip3 xs ys zs
         let x, y, z = ks.Head
         x + y + z
      @@>