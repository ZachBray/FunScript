[<NUnit.Framework.TestFixture>] 
module FunJS.Tests.Lists

open NUnit.Framework


[<Test>]
let ``List literals work``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         ()
      @@>

[<Test>]
let ``List.Length works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.Length |> float
      @@>

[<Test>]
let ``List.IsEmpty works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.IsEmpty
      @@>

[<Test>]
let ``List.Head works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.Head
      @@>

[<Test>]
let ``List.Tail works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.Tail.Head
      @@>

[<Test>]
let ``List.Item works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs.[3]
      @@>

[<Test>]
let ``List cons works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = 0. :: xs
         let zs = List.Cons(0., xs)
         ys.Head + xs.Head
      @@>

[<Test>]
let ``List.empty works``() =
   check  
      <@@ 
         let xs = 1. :: List.Empty
         let ys = 1. :: List.empty
         xs.Head + ys.Head
      @@>

[<Test>]
let ``List.append works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [0.]
         let zs = List.append ys xs
         zs.Head + zs.Tail.Head
      @@>

[<Test>]
let ``List.average works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.average xs
      @@>

[<Test>]
let ``List.averageBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.averageBy ((*) 2.) xs
      @@>

[<Test>]
let ``List.choose works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let result = xs |> List.choose (fun x ->
            if x > 2. then Some x
            else None) 
         result.Head + result.Tail.Head
      @@>

[<Test>]
let ``List.collect works``() =
   check  
      <@@ 
         let xs = [[1.]; [2.]; [3.]; [4.]]
         let ys = xs |> List.collect id
         ys.Head  + ys.Tail.Head
      @@>

let ``List.concat works``() =
   check  
      <@@ 
         let xs = [[1.]; [2.]; [3.]; [4.]]
         let ys = xs |> List.concat
         ys.Head  + ys.Tail.Head
      @@>

[<Test>]
let ``List.exists works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> List.exists (fun x -> x = 2.)
      @@>

[<Test>]
let ``List.exists2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         List.exists2 (fun x y -> x * y = 16.) xs ys
      @@>

[<Test>]
let ``List.filter works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = xs |> List.filter (fun x -> x > 5.)
         ys.IsEmpty
      @@>

[<Test>]
let ``List.find works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> List.find ((=) 2.)
      @@>

[<Test>]
let ``List.findIndex works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> List.findIndex ((=) 2.)
         |> float
      @@>

[<Test>]
let ``List.fold works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = xs |> List.fold (+) 0.
         total
      @@>

[<Test>]
let ``List.fold2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         let total = List.fold2 (fun x y z -> x + y + z) 0. xs ys
         total
      @@>

[<Test>]
let ``List.foldBack works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = List.foldBack (+) xs 0.
         total
      @@>

[<Test>]
let ``List.foldBack2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         let total = List.foldBack2 (fun x y z -> x + y + z) xs ys 0.
         total
      @@>

[<Test>]
let ``List.forall works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.forall (fun x -> x < 5.) xs
      @@>

[<Test>]
let ``List.forall2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         List.forall2 (=) xs ys
      @@>

[<Test>]
let ``List.head works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.head xs
      @@>

[<Test>]
let ``List.init works``() =
   check  
      <@@ 
         let xs = List.init 4 float
         xs.Head + xs.Tail.Head
      @@>

[<Test>]
let ``List.isEmpty works``() =
   check  
      <@@ 
         let xs = [1]
         List.isEmpty xs
      @@>

[<Test>]
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

[<Test>]
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

[<Test>]
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

[<Test>]
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

[<Test>]
let ``List.length works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         List.length xs 
         |> float
      @@>

[<Test>]
let ``List.map works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = xs |> List.map ((*) 2.)
         ys.Head
      @@>

[<Test>]
let ``List.map2 works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = [2.]
         let zs = List.map2 (*) xs ys
         zs.Head
      @@>

[<Test>]
let ``List.map3 works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = [2.]
         let zs = [3.]
         let ks = List.map3 (fun x y z -> x * y * z) xs ys zs
         zs.Head
      @@>

[<Test>]
let ``List.mapi works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = xs |> List.mapi (fun i x -> float i + x)
         ys.Head
      @@>

[<Test>]
let ``List.mapi2 works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = [2.]
         let zs = List.mapi2 (fun i x y -> float i + x * y) xs ys
         zs.Head
      @@>

[<Test>]
let ``List.max works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.max
      @@>

[<Test>]
let ``List.maxBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.maxBy (fun x -> -x)
      @@>

[<Test>]
let ``List.min works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.min
      @@>

[<Test>]
let ``List.minBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.minBy (fun x -> -x)
      @@>

[<Test>]
let ``List.nth works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         List.nth xs 1
      @@>

[<Test>]
let ``List.ofArray works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = List.ofArray xs
         ys.Head
      @@>

[<Test>]
let ``List.ofSeq works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|] :> _ seq
         let ys = List.ofSeq xs
         ys.Head
      @@>

[<Test>]
let ``List.partition works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys, zs = xs |> List.partition (fun x -> x <= 1.)
         ys.Head - zs.Head
      @@>

[<Test>]
let ``List.permute works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.permute (fun i -> i + 1 - 2 * (i % 2))
         ys.Head
      @@>

[<Test>]
let ``List.pick works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.pick (fun x ->
            match x with
            | 2. -> Some x
            | _ -> None)
      @@>

[<Test>]
let ``List.reduce works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.reduce (+)
      @@>

[<Test>]
let ``List.reduceBack works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.reduceBack (+)
      @@>

[<Test>]
let ``List.replicate works``() =
   check  
      <@@ 
         let xs = List.replicate 2 1.
         xs.Head + xs.Tail.Head
      @@>

[<Test>]
let ``List.rev works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.rev
         ys.Head
      @@>

[<Test>]
let ``List.scan works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = xs |> List.scan (+) 0.
         ys.Head + ys.Tail.Head
      @@>

[<Test>]
let ``List.scanBack works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = List.scanBack (+) xs 0.
         ys.Head + ys.Tail.Head
      @@>

[<Test>]
let ``List.sort works``() =
   check  
      <@@ 
         let xs = [3.; 4.; 1.; 2.]
         let ys = xs |> List.sort
         ys.Head + ys.Tail.Head
      @@>

[<Test>]
let ``List.sortBy works``() =
   check  
      <@@ 
         let xs = [3.; 1.; 4.; 2.]
         let ys = xs |> List.sortBy (fun x -> -x)
         ys.Head + ys.Tail.Head
      @@>

[<Test>]
let ``List.sortWith works``() =
   check  
      <@@ 
         let xs = [3.; 4.; 1.; 2.]
         let ys = xs |> List.sortWith (fun x y -> int(x - y))
         ys.Head + ys.Tail.Head
      @@>

[<Test>]
let ``List.sum works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.sum
      @@>

[<Test>]
let ``List.sumBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> List.sumBy ((*) 2.)
      @@>

[<Test>]
let ``List.tail works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.tail
         ys.Head
      @@>

[<Test>]
let ``List.toArray works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.toArray
         ys.[0] + ys.[1]
      @@>

[<Test>]
let ``List.toSeq works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.toSeq
         ys.GetEnumerator().MoveNext()
      @@>

[<Test>]
let ``List.tryFind works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.tryFind ((=) 1.)
         ys.IsSome
      @@>

[<Test>]
let ``List.tryFindIndex works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> List.tryFindIndex ((=) 2.)
         ys.Value |> float
      @@>

[<Test>]
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

[<Test>]
let ``List.unzip works``() =
   check  
      <@@ 
         let xs = [1., 2.]
         let ys, zs = xs |> List.unzip
         ys.Head + zs.Head
      @@>

[<Test>]
let ``List.unzip3 works``() =
   check  
      <@@ 
         let xs = [1., 2., 3.]
         let ys, zs, ks = xs |> List.unzip3
         ys.Head + zs.Head + ks.Head
      @@>

[<Test>]
let ``List.zip works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = [1.; 2.; 3.]
         let zs = List.zip xs ys
         let x, y = zs.Head
         x + y
      @@>

[<Test>]
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