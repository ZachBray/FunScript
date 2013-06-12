module (*internal*) FunScript.InternalCompiler

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
   abstract ReplacementFor: MethodBase -> Quote.CallType -> MethodInfo option
   abstract NextTempVar: unit -> Var
   abstract DefineGlobal: string -> (Var -> JSStatement list) -> Var
   abstract DefineGlobalInitialization: JSStatement list -> unit
   abstract Globals: JSStatement list
   abstract ShouldFlattenGenericsForReflection: bool

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

type Compiler(components, shouldFlattenGenericsForReflection) as this = 
   let key (mb:MethodBase) tt =
      mb.Name, mb.DeclaringType.Name, mb.DeclaringType.Namespace, tt
 
   let callerReplacers =
      components |> Seq.choose (function
         | CallReplacer r -> Some (key r.Target r.TargetType, r)
         | _ -> None) |> Map.ofSeq

   let callerReplacements =
      callerReplacers |> Seq.choose (fun (KeyValue(id, r)) -> 
         r.Replacement |> Option.map (fun replacement ->
            key r.Target r.TargetType, replacement))
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
      if mi.IsConstructor then mi.DeclaringType.GetGenericArguments()
      else
         Array.append
            (mi.DeclaringType.GetGenericArguments())
            (mi.GetGenericArguments())

   let mutable usedMethods = []

   let remember (methodBase:MethodBase) =
      usedMethods <- methodBase :: usedMethods

   let tryCompileCall callType returnStrategy mi obj exprs  =
      let this = this :> ICompiler
      remember mi
      match callerReplacers.TryFind (key mi callType) with
      | Some r ->
         let typeArgs = getTypeArgs mi
         r.TryReplace this returnStrategy (obj, typeArgs, exprs)
      | None -> []
         
   let compile returnStategy expr =
      let replacementResult =
         match Quote.tryToMethodBase expr with
         | Some (obj, mi, exprs, callType) ->
            match Quote.specialOp mi with
            | Some opMi -> 
               match tryCompileCall callType returnStategy opMi obj exprs with
               | [] -> tryCompileCall callType returnStategy mi obj exprs
               | stmts -> stmts
            | None -> tryCompileCall callType returnStategy mi obj exprs
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
      | Some (var, _) -> var
      | None -> 
         // Define upfront to avoid problems with mutually recursive methods
         let var = Var.Global(name, typeof<obj>)
         globals <- globals |> Map.add name (var, [])
         let assignment = cons var
         globals <- globals |> Map.add name (var, assignment)
         var

   let mutable initialization = List.empty

   let getGlobals() =
      let globals = globals |> Map.toList |> List.map snd

      let declarations = 
         match globals with
         | [] -> []
         | _ -> [Declare (globals |> List.map (fun (var, _) -> var))]

      let assignments = globals |> List.collect snd

      List.append
         (List.append declarations assignments)
         initialization

   member __.Compile returnStategy expr  = 
      compile returnStategy expr

   member __.Globals = getGlobals()

   interface ICompiler with

      member __.Compile returnStategy expr = 
         compile returnStategy expr

      member __.ReplacementFor (pi:MethodBase) targetType =
         callerReplacements.TryFind (key pi targetType)

      member __.NextTempVar() = 
         incr nextId
         Var(sprintf "_temp%i" !nextId, typeof<obj>, false) 
         
      member __.DefineGlobal name cons =
         define name cons

      member __.DefineGlobalInitialization stmts =
         initialization <- List.append initialization stmts

      member __.Globals = getGlobals()

      member __.ShouldFlattenGenericsForReflection = shouldFlattenGenericsForReflection
         