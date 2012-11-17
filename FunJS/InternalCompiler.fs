module internal FunJS.InternalCompiler

open AST
open System
open System.Reflection
open Microsoft.FSharp.Quotations

type Helpers =
   static member Cast(x:obj) : 'a = failwith "never"

type IReturnStrategy =
   abstract Return: JSExpr -> JSStatement

type ICompiler = 
   abstract Compile: returnStategy:IReturnStrategy -> expr:Expr -> JSStatement list
//   abstract CompileCall: 
//      returnStategy:IReturnStrategy -> 
//      MethodBase -> Expr option -> Expr list ->
//      JSStatement list
   abstract ReplacementFor: MethodBase -> Quote.CallType -> MethodInfo option
   abstract NextTempVar: unit -> Var
   abstract UsedMethods: MethodBase list
   abstract DefineGlobal: string -> (unit -> Var list * JSBlock) -> Var
   abstract Globals: JSStatement list

type ICompilerComponent =
   abstract TryCompile: compiler:ICompiler -> returnStategy:IReturnStrategy -> expr:Expr -> JSStatement list

type CallReplacer = 
  { Target: MethodBase
    TargetType: Quote.CallType
    Replacement: MethodInfo option
    TryReplace: ICompiler -> IReturnStrategy -> Expr option * Type [] * Expr list -> JSStatement list }

type CompilerComponent =
   | CallReplacer of CallReplacer
   | CompilerComponent of ICompilerComponent

type Compiler(components) as this =  
   let callerReplacers =
      components |> Seq.choose (function
         | CallReplacer r -> Some ((r.Target.Name, r.Target.MetadataToken, r.TargetType), r)
         | _ -> None) |> Map.ofSeq

   let callerReplacements =
      callerReplacers |> Seq.choose (fun (KeyValue(id, r)) -> 
         r.Replacement |> Option.map (fun replacement ->
            (r.Target.Name, r.Target.MetadataToken, r.TargetType), replacement))
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

   let getTypeArgs(mi:MethodBase) =
      Array.append
         (mi.DeclaringType.GetGenericArguments())
         (mi.GetGenericArguments())

   let mutable usedMethods = []

   let remember (methodBase:MethodBase) =
      usedMethods <- methodBase :: usedMethods

   let tryCompileCall callType returnStategy mi obj exprs  =
      let this = this :> ICompiler
      remember mi
      match callerReplacers.TryFind (mi.Name, mi.MetadataToken, callType) with
      | Some r ->
         let typeArgs = getTypeArgs mi
         r.TryReplace this returnStategy (obj, typeArgs, exprs)
      | None -> []

   let compile returnStategy expr =
      let replacementResult =
         match Quote.tryToMethodBase expr with
         | Some (obj, mi, exprs, callType) ->
            tryCompileCall callType returnStategy mi obj exprs
         | None -> []
      let result =
         match replacementResult with
         | [] -> tryAllComponents returnStategy expr
         | statements -> statements
      match result with
      | [] -> failwithf "Could not compile expression: %A" expr
      | statements -> statements

   let nextId = ref 0

   let mutable globals = Map.empty

   let define name cons =
      match globals |> Map.tryFind name with
      | Some (var, _, _) -> var
      | None -> 
         // Define upfront to avoid problems with mutually recursive methods
         let var = Var.Global(name, typeof<obj>)
         globals <- globals |> Map.add name (var, [], Block [])
         let argVars, bodyExpr = cons()
         globals <- globals |> Map.add name (var, argVars, bodyExpr)
         var

   let getGlobals() =
      let globals = globals |> Map.toList |> List.map snd

      let declarations = 
         match globals with
         | [] -> []
         | _ -> [Declare (globals |> List.map (fun (var, _,_) -> var))]

      let assignments =
         globals |> List.map (fun (var, argVars, bodyExpr) ->
            Assign(Reference var, Lambda(argVars, bodyExpr)))
      List.append declarations assignments

   member __.Compile returnStategy expr  = 
      compile returnStategy expr

   member __.Globals = getGlobals()

   interface ICompiler with

      member __.Compile returnStategy expr = 
         compile returnStategy expr

      member __.ReplacementFor (pi:MethodBase) targetType =
         let key = pi.Name, pi.MetadataToken, targetType
         callerReplacements.TryFind key

      member __.NextTempVar() = 
         incr nextId
         Var(sprintf "_temp%i" !nextId, typeof<obj>, false) 

      member __.UsedMethods = usedMethods

      member __.DefineGlobal name cons =
         define name cons

      member __.Globals = getGlobals()
         