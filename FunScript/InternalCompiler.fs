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
   let parameterKey (mb : MethodBase) =
      mb.GetParameters() |> Array.map (fun pi ->
         pi.ParameterType.Name)
   let key (mb:MethodBase) tt =
      mb.Name, mb.DeclaringType.Name, mb.DeclaringType.Namespace, tt
 
   let callerReplacers =
      components |> Seq.choose (function
         | CallReplacer r -> Some r
         | _ -> None) 
      |> Seq.groupBy (fun r -> key r.Target r.TargetType)
      |> Seq.map (fun (key, values) ->
         key, values |> Seq.map (fun r -> parameterKey r.Target, r) |> Map.ofSeq)
      |> Map.ofSeq

   let callerReplacements =
      callerReplacers |> Seq.choose (fun (KeyValue(id, rs)) ->
         let replacements =
            rs 
            |> Map.map (fun k r -> r.Replacement)
            |> Map.filter (fun k r -> r.IsSome)
            |> Map.map (fun k r -> r.Value)
         if replacements = Map.empty then
            None
         else Some (id, replacements))
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

   let tryCompileCall callType returnStrategy mi obj exprs  =
      let this = this :> ICompiler
      match callerReplacers.TryFind (key mi callType) with
      | Some rs ->
         let paramKey = parameterKey mi
         let r =
            match rs.TryFind paramKey with
            | None -> (rs |> Seq.head).Value
            | Some r -> r
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
         |> Option.map (fun rs ->
            let paramKey = parameterKey pi
            let r =
               match rs.TryFind paramKey with
               | None -> (rs |> Seq.head).Value
               | Some r -> r
            r)

      member __.NextTempVar() = 
         incr nextId
         Var(sprintf "_%i" !nextId, typeof<obj>, false) 
         
      member __.DefineGlobal name cons =
         define name cons

      member __.DefineGlobalInitialization stmts =
         initialization <- List.append initialization stmts

      member __.Globals = getGlobals()

      member __.ShouldFlattenGenericsForReflection = shouldFlattenGenericsForReflection
         