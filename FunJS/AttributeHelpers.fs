[<AutoOpen>]
module internal FunJS.AttributeHelpers

open System.Reflection

let private orDefault maybeX =
   match maybeX with
   | Some x -> x
   | None -> Unchecked.defaultof<_>

let private findAttribute<'attr> =
   Array.tryPick (fun (obj:obj) -> 
      match obj with
      | :? 'attr as attr -> Some attr
      | _ -> None)
   >> orDefault

type System.Type with
   member t.GetCustomAttribute<'attr>() =
      t.GetCustomAttributes(true)
      |> findAttribute<'attr>
      
type System.Reflection.MethodBase with
   member t.GetCustomAttribute<'attr>() =
      t.GetCustomAttributes(true)
      |> findAttribute<'attr>
      
type System.Reflection.PropertyInfo with
   member t.GetCustomAttribute<'attr>() =
      t.GetCustomAttributes(true)
      |> findAttribute<'attr>
      