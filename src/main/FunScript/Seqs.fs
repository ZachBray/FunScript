module internal FunScript.Seqs

open AST
open System.Collections
open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp.Quotations


let seqImplementations =
    dict [
        typeof<list<obj>>.Name,                             <@@ Core.Seq.OfList @@>
        typeof<ResizeArray<obj>>.Name,                      <@@ Core.ResizeArray.ToSeq @@>
        typeof<ICollection<obj>>.Name,                      <@@ Core.Dictionaries.MutableDic.CollToSeq @@>
        typeof<IDictionary<obj, obj>>.Name,                 <@@ Core.Dictionaries.MutableDic.DicToSeq @@>
        typeof<Dictionary<obj, obj>>.Name,                  <@@ Core.Dictionaries.MutableDic.DicToSeq @@>
        typeof<Dictionary<obj, obj>.KeyCollection>.Name,    <@@ Core.Dictionaries.MutableDic.KeysToSeq @@>
        typeof<Dictionary<obj, obj>.ValueCollection>.Name,  <@@ Core.Dictionaries.MutableDic.ValuesToSeq @@>
        typeof<MatchCollection>.Name,                       <@@ Core.Regex.MatchCollectionToSeq @@>
        typeof<GroupCollection>.Name,                       <@@ Core.Regex.GroupCollectionToSeq @@>
    ]

let private getSpecificType (baseNonGeneric: System.Type) (baseGeneric: System.Type) (expr: Expr) =
    let genericType = baseGeneric.GetGenericTypeDefinition()
    let genericArgs = expr.Type.GetGenericArguments()
    match genericArgs.Length with
    | 0 -> baseNonGeneric
    | 1 -> genericType.MakeGenericType(genericArgs)
    | 2 when expr.Type.FullName.Contains(typeof<Dictionary<_,_>.KeyCollection>.Name) -> genericType.MakeGenericType(genericArgs.[0])
    | 2 when expr.Type.FullName.Contains(typeof<Dictionary<_,_>.ValueCollection>.Name) -> genericType.MakeGenericType(genericArgs.[1])
    | 2 -> let kvType = typeof<KeyValuePair<obj,obj>>.GetGenericTypeDefinition().MakeGenericType(genericArgs)
           genericType.MakeGenericType(kvType)
    | _ -> failwith "Enumerables with more than 2 generic arguments are not supported"

let private compileGenericMethod (compiler: InternalCompiler.ICompiler) returnStrategy (genericExpr: Expr) quote =
    let genericArguments = genericExpr.Type.GetGenericArguments()
    let mi =
        if genericArguments.Length = 0
        then let mi, _ = Quote.toMethodInfoFromLambdas quote in mi
        else let mi, _ = Quote.toMethodInfoFromLambdas quote in mi.MakeGenericMethod(genericArguments)
    compiler.Compile returnStrategy (Expr.Call(mi, [genericExpr]))

// NOTE: Force all types implementing GetEnumerator be coerced to seq so we don't need to worry about compiling many different methods
let private enumComponents =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.Call(Some(expr),mi,args) when mi.Name = "GetEnumerator" && expr.Type.Name.IndexOf(typeof<IEnumerable>.Name) <> 0 ->
            let enumType = getSpecificType typeof<IEnumerable> typeof<IEnumerable<obj>> expr
            compiler.Compile returnStrategy (Expr.Call(Expr.Coerce(expr, enumType), enumType.GetMethod("GetEnumerator"), []))
      | Patterns.Call(Some(expr),mi,args) when mi.Name = "MoveNext" && expr.Type.Name.IndexOf(typeof<IEnumerator>.Name) <> 0 ->
            compiler.Compile returnStrategy (Expr.Call(Expr.Coerce(expr, typeof<IEnumerator>), typeof<IEnumerator>.GetMethod("MoveNext"), []))
      | Patterns.PropertyGet(Some(expr),pi,args) when pi.Name = "Current" && expr.Type.Name.IndexOf(typeof<IEnumerator>.Name) <> 0 ->
            let enumType = getSpecificType typeof<IEnumerator> typeof<IEnumerator<obj>> expr
            compiler.Compile returnStrategy (Expr.PropertyGet(Expr.Coerce(expr, enumType), enumType.GetProperty("Current"), []))
      | _ -> []

let private toSeq =
   CompilerComponent.create <| fun (|Split|) compiler returnStrategy ->
      function
      | Patterns.Coerce(expr, t) 
         when t.Name = typeof<seq<obj>>.Name || t.Name = typeof<IEnumerable>.Name ->
         if expr.Type.IsArray
         then
            let mi, _ = Quote.toMethodInfoFromLambdas <@@ Core.Seq.OfArray @@>
            let specificMi = mi.MakeGenericMethod [|expr.Type.GetElementType()|]
            compiler.Compile returnStrategy (Expr.Call(specificMi, [expr]))
         else
            if seqImplementations.ContainsKey(expr.Type.Name)
            then compileGenericMethod compiler returnStrategy expr seqImplementations.[expr.Type.Name]
            else []      
      | _ -> []

let components = 
   [
      [
         toSeq
         enumComponents
         //ExpressionReplacer.create <@ seq @> <@ (fun xs -> xs :> seq<_>) @>
      ]
      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Collections.SeqModule"
         "FunScript" "FunScript.Core.Seq"

      ExpressionReplacer.createModuleMapping 
         "FSharp.Core" "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers"
         "FunScript" "FunScript.Core.Seq.RuntimeHelpers"
   ] |> List.concat
