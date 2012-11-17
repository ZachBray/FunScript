module internal FunJS.Quote

open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

type CallType =
   | MethodCall
   | UnionCaseConstructorCall
   | ConstructorCall

let private flags =
   BindingFlags.Public |||  
   BindingFlags.NonPublic |||
   BindingFlags.Instance |||
   BindingFlags.Static

let getCaseMethodInfo (uci:UnionCaseInfo) =
   let unionType = uci.DeclaringType
   match unionType.GetMember(uci.Name, flags) with
   | [| :? System.Type as caseType |] -> 
      caseType.GetConstructors(flags).[0]
      :> MethodBase, caseType
   | [| :? MethodInfo as mi |] -> mi :> MethodBase, unionType
   | [| :? PropertyInfo as pi |] -> pi.GetMethod :> MethodBase, unionType
   | _ ->
      match unionType.GetMember("New" + uci.Name, flags) with
      | [| :? MethodInfo as mi |] -> mi :> MethodBase, unionType
      | [| :? PropertyInfo as pi |] -> pi.GetMethod :> MethodBase, unionType
      | _ -> failwith "never"

let tryToMethodBase = function
   | Patterns.Call(obj,mi,args) -> 
      Some(obj, mi :> MethodBase, args, MethodCall)
   | Patterns.PropertyGet(obj,pi,args) -> 
      Some(obj, pi.GetMethod :> MethodBase, args, MethodCall) 
   | Patterns.PropertySet(obj,pi,args,v) -> 
      Some(obj, pi.SetMethod :> MethodBase, List.append args [v], MethodCall)
   | Patterns.NewUnionCase(uci, exprs) -> 
      Some(None, fst <| getCaseMethodInfo uci, exprs, UnionCaseConstructorCall)
   | Patterns.NewObject(ci, exprs) ->
      Some(None, ci :> MethodBase, exprs, ConstructorCall)
   | _ -> None

let (|MethodBase|_|) = tryToMethodBase

let tryToMethodBaseFromLambdas expr =
   match expr with
   | MethodBase mb
   | DerivedPatterns.Lambdas(_, MethodBase mb) -> Some mb
   | _ -> None

let private mostGeneric(mi:MethodInfo)  = 
   if mi.IsGenericMethod then mi.GetGenericMethodDefinition()
   else mi

let toMethodInfoFromLambdas expr =
   match tryToMethodBaseFromLambdas expr with
   | Some (_, (:? MethodInfo as mi), _, callType) -> mostGeneric mi, callType
   | Some _ | None -> failwith "Expected a method/property call/get/set wrapped in a lambda"

let toMethodBaseFromLambdas expr =
   match tryToMethodBaseFromLambdas expr with
   | Some (_, mi, _, callType) -> mi, callType
   | None -> failwith "Expected a method/property call/get/set wrapped in a lambda"