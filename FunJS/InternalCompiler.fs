module internal FunJS.InternalCompiler

open AST
open System
open System.Reflection
open Microsoft.FSharp.Quotations

type IReturnStrategy =
   abstract Return: JSExpr -> JSStatement

type ICompiler = 
   abstract Compile: returnStategy:IReturnStrategy -> expr:Expr -> JSStatement list
   abstract ReplacementFor: MethodBase -> MethodInfo option
   abstract NextTempVar: unit -> Var
   abstract UsedMethods: MethodBase list

type ICompilerComponent =
   abstract TryCompile: compiler:ICompiler -> returnStategy:IReturnStrategy -> expr:Expr -> JSStatement list
   
type token = int

type MethodIdentifier = 
   | Getter of token
   | Setter of token
   | Method of token

type PropertyGetReplacer = 
  { Target: PropertyInfo
    Replacement: MethodInfo option
    TryReplace: ICompiler -> IReturnStrategy -> Expr option * Type [] * Expr list -> JSStatement list }

type PropertySetReplacer = 
  { Target: PropertyInfo
    Replacement: MethodInfo option
    TryReplace: ICompiler -> IReturnStrategy -> Expr option * Type [] * Expr list * Expr -> JSStatement list }

type MethodCallReplacer = 
  { Target: MethodInfo
    Replacement: MethodInfo option
    TryReplace: ICompiler -> IReturnStrategy -> Expr option * Type [] * Expr list -> JSStatement list }

type CompilerComponent =
   | PropertyGetReplacer of PropertyGetReplacer
   | PropertySetReplacer of PropertySetReplacer
   | MethodCallReplacer of MethodCallReplacer
   | CompilerComponent of ICompilerComponent

type Compiler(components) as this =
   let getterReplacers = 
      components |> Seq.choose (function
         | PropertyGetReplacer r -> Some ((r.Target.Name, r.Target.MetadataToken), r)
         | _ -> None) |> Map.ofSeq

   let getterReplacements =
      getterReplacers |> Seq.choose (fun (KeyValue(id, r)) -> 
         r.Replacement |> Option.map (fun replacement ->
            (r.Target.GetMethod.Name, r.Target.GetMethod.MetadataToken), replacement))
      |> Map.ofSeq

   let setterReplacers =
      components |> Seq.choose (function
         | PropertySetReplacer r -> Some ((r.Target.Name, r.Target.MetadataToken), r)
         | _ -> None) |> Map.ofSeq

   let setterReplacements =
      setterReplacers |> Seq.choose (fun (KeyValue(id, r)) -> 
         r.Replacement |> Option.map (fun replacement ->
            (r.Target.SetMethod.Name, r.Target.SetMethod.MetadataToken), replacement))
      |> Map.ofSeq
   
   let callerReplacers =
      components |> Seq.choose (function
         | MethodCallReplacer r -> Some ((r.Target.Name, r.Target.MetadataToken), r)
         | _ -> None) |> Map.ofSeq

   let callerReplacements =
      callerReplacers |> Seq.choose (fun (KeyValue(id, r)) -> 
         r.Replacement |> Option.map (fun replacement ->
            (r.Target.Name, r.Target.MetadataToken), replacement))
      |> Map.ofSeq

   let rest = 
      components |> Seq.choose (function
         | CompilerComponent c -> Some c
         | _ -> None) |> Seq.toList

   let tryComponent returnStategy expr (part:ICompilerComponent) =
      match part.TryCompile this returnStategy expr with
      | [] -> None
      | procCodes -> Some procCodes

   let tryAllComponents returnStategy expr =
      let result = 
         rest |> List.tryPick(tryComponent returnStategy expr)
      match result with
      | None -> []
      | Some statements -> statements

   let getTypeArgs(mi:MethodInfo) =
      Array.append
         (mi.DeclaringType.GetGenericArguments())
         (mi.GetGenericArguments())

   let mutable usedMethods = []

   let remember (methodBase:MethodBase) =
      usedMethods <- methodBase :: usedMethods

   let compile returnStategy expr =
      let replacementResult =
         let this = this :> ICompiler
         match expr with
         | Patterns.PropertyGet(obj,pi,exprs) -> 
            match getterReplacers.TryFind (pi.Name, pi.MetadataToken) with
            | Some r -> 
               remember pi.GetMethod
               let typeArgs = getTypeArgs pi.GetMethod
               r.TryReplace this returnStategy (obj, typeArgs, exprs)
            | None -> []
         | Patterns.PropertySet(obj,pi,exprs,v) ->
            match setterReplacers.TryFind (pi.Name, pi.MetadataToken) with
            | Some r -> 
               remember pi.SetMethod
               let typeArgs = getTypeArgs pi.SetMethod
               r.TryReplace this returnStategy (obj, typeArgs, exprs, v)
            | None -> []
         | Patterns.Call(obj,mi,exprs) ->
            remember mi
            let typeArgs = getTypeArgs mi
            match callerReplacers.TryFind (mi.Name, mi.MetadataToken) with
            | Some r -> r.TryReplace this returnStategy (obj, typeArgs, exprs)
            | None -> []
         | _ -> []
      let result =
         match replacementResult with
         | [] -> tryAllComponents returnStategy expr
         | statements -> statements
      match result with
      | [] -> failwithf "Could not compile expression: %A" expr
      | statements -> statements

   let nextId = ref 0

   member __.Compile returnStategy expr  = 
      compile returnStategy expr

   interface ICompiler with

      member __.Compile returnStategy expr = 
         compile returnStategy expr

      member __.ReplacementFor (pi:MethodBase) =
         let orElse f x =
            match x with 
            | Some x -> Some x
            | None -> f()
         let key = pi.Name, pi.MetadataToken
         getterReplacements.TryFind key
         |> orElse (fun () ->
            setterReplacements.TryFind key
            |> orElse (fun () ->
               callerReplacements.TryFind key
            )
         )

      member __.NextTempVar() = 
         incr nextId
         Var(sprintf "_temp%i" !nextId, typeof<obj>, false) 

      member __.UsedMethods = usedMethods

