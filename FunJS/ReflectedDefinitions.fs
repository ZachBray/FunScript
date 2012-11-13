module internal FunJS.ReflectedDefinitions

open AST
open Microsoft.FSharp.Quotations
open System.Reflection

let private sanitize(str:string) =
   str.Replace('.', '_').Replace('+', '_')

let private getJSTypeName(t:System.Type) =
   sanitize t.Name

let private getJSName(mi:MethodBase) =
   let prefix = 
      if mi.IsStatic then ""
      else "i_"
   prefix + getJSTypeName mi.DeclaringType + "_" + sanitize mi.Name

let private (|List|) = Option.toList

let private (|NonNull|_|) x = 
   if obj.ReferenceEquals(x, null) then None
   else Some x

let private (|ReflectedDefinition|_|) mi = 
   match Expr.TryGetReflectedDefinition mi with
   | Some _ -> Some(getJSName mi)
   | _ -> None

let private rootInterfaceName = typeof<IJSRoot>.Name
let private mappingInterfaceName = typeof<IJSMapping>.Name

let rec private buildRootWalk acc (t:System.Type) =
   match t with
   | NonNull t ->
      match t.GetInterface rootInterfaceName with
      | NonNull _ -> acc
      | _ -> buildRootWalk (t.Name :: acc) t.DeclaringType
   | _ -> failwith "An IJSMapping interface had no root"

let private removePropertyPrefixes(name:string) =
   if name.StartsWith "get_" || name.StartsWith "set_" then 
      name.Substring(4), true
   else name, false

let private (|JSMapping|_|) (mi:MethodBase) = 
   let mappingInterface = 
      mi.DeclaringType.GetInterface mappingInterfaceName
   match mappingInterface with
   | NonNull _ -> 
      let name, isProperty = removePropertyPrefixes mi.Name
      let isStatic = mi.IsStatic
      if not isStatic then Some (name, isStatic, isProperty)
      else 
         let rootWalk = buildRootWalk [] mi.DeclaringType
         match rootWalk with
         | [] -> Some (name, isStatic, isProperty)
         | _ ->
            let prefix = rootWalk |> String.concat "."
            Some (sprintf "%s.%s" prefix name, isStatic, isProperty)
   | _ -> None

let private createCall (|Split|) (returnStategy:InternalCompiler.IReturnStrategy) exprs mi =
   let exprs = exprs |> List.concat
   let decls, refs = 
      exprs 
      |> List.map (fun (Split(valDecl, valRef)) -> valDecl, valRef)
      |> List.unzip
   match mi, refs with
   | JSMapping(name, true, false), _
   | ReflectedDefinition name, _ ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| Apply(Reference (Var.Global(name, typeof<obj>)), refs) ]
   | JSMapping(name, true, true), [] ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| Reference (Var.Global(name, typeof<obj>)) ]
   | JSMapping("Invoke", false, false), instance::refs ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| Apply(instance, refs) ]
   | JSMapping(name, false, false), instance::refs ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| Apply(PropertyGet(instance, name), refs) ]
   | JSMapping(name, false, true), instance::[] ->
      [ yield! decls |> List.concat
        yield returnStategy.Return <| PropertyGet(instance, name) ]
   | _ -> []

let private methodCalling =
   CompilerComponent.create <| fun split _ returnStategy ->
      function
      | Patterns.Call(List objExpr, mi, exprs) -> createCall split returnStategy [objExpr; exprs] mi
      | _ -> []

let private propertyGetting =
   CompilerComponent.create <| fun split _ returnStategy ->
      function
      | Patterns.PropertyGet(List objExpr, pi, exprs) ->
         createCall split returnStategy [objExpr; exprs] pi.GetMethod
      | _ -> []
      
let private propertySetting =
   CompilerComponent.create <| fun split _ returnStategy ->
      function
      | Patterns.PropertySet(List objExpr, pi, exprs, valExpr) ->
         createCall split returnStategy [objExpr; exprs; [valExpr]] pi.SetMethod
      | _ -> []

let private fieldGetting =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.FieldGet(Some(Split(objDecl, objRef)), fi) ->
         [ yield! objDecl
           yield returnStategy.Return <| PropertyGet(objRef, fi.Name) ]
      | Patterns.FieldGet(None, fi) ->
         let name = getJSTypeName fi.DeclaringType
         [ yield returnStategy.Return <| PropertyGet(Reference (Var.Global(name, typeof<obj>)), fi.Name) ]
      | _ -> []

let private fieldSetting =
   CompilerComponent.create <| fun (|Split|) _ returnStategy ->
      function
      | Patterns.FieldSet(Some(Split(objDecl, objRef)), fi, Split(valDecl, valRef)) ->
         [ yield! objDecl
           yield! valDecl
           yield Assign(PropertyGet(objRef, fi.Name), valRef) ]
      | Patterns.FieldSet(None, fi, Split(valDecl, valRef)) ->
         let name = getJSTypeName fi.DeclaringType
         [ yield! valDecl
           yield Assign(PropertyGet(Reference (Var.Global(name, typeof<obj>)), fi.Name), valRef) ]
      | _ -> []

let private objectGuid = typeof<obj>.GUID

let private constructingInstances =
   CompilerComponent.create <| fun split _ returnStategy ->
      function
      | Patterns.NewObject(ci, exprs) as expr -> 
         if ci.DeclaringType.GUID = objectGuid then
            [ Scope <| Block [] ]
         else
            createCall split returnStategy [exprs] ci
      | _ -> []

open System.Collections.Generic

module private Expr =
   let iter f expr =
      let rec iter expr =
         f expr
         match expr with
         | ExprShape.ShapeVar _ -> ()
         | ExprShape.ShapeLambda(_, expr) -> iter expr
         | ExprShape.ShapeCombination (_, exprs) ->
            exprs |> List.iter iter
      iter expr


open Microsoft.FSharp.Reflection

let private findUsedMethods expr (compiler:InternalCompiler.ICompiler) =
   let found = Dictionary()

   let (|HasDefinition|_|) (mb:MethodBase) =
      let finalMi = 
         match compiler.ReplacementFor mb with
         | None -> mb
         | Some mi -> mi :> MethodBase
      match finalMi with
      | DerivedPatterns.MethodWithReflectedDefinition expr -> Some(finalMi, expr)
      | _ -> None

   let rec find expr =
      let add mi =
         match mi with
         | HasDefinition(mi, DerivedPatterns.Lambdas(vars, bodyExpr)) ->
            let name = getJSName mi
            if not <| found.ContainsKey name then
               found.Add(name, (mi, vars |> List.concat, bodyExpr))
               find bodyExpr
         | HasDefinition(mi, unitLambdaBodyExpr) ->
            let name = getJSName mi
            if not <| found.ContainsKey name then
               found.Add(name, (mi, [], unitLambdaBodyExpr))
               find unitLambdaBodyExpr
         | _ -> () //TODO: throw? or is this for expression replacer stuff?
      
      expr |> Expr.iter (function
         | Patterns.Call(_, mi, _) -> add mi
         | Patterns.PropertyGet(_, pi, _) -> 
            match pi.GetMethod with
            | NonNull mi -> add mi
            | _ -> invalidOp ""
         | Patterns.PropertySet(_, pi, _, _) ->
            match pi.SetMethod with
            | NonNull mi -> add mi
            | _ -> invalidOp ""
         | Patterns.NewObject(ci, _) -> add ci
         | _ -> ())
   find expr
   found

let private genMethod (mb:MethodBase) (vars:Var list) bodyExpr (compiler:InternalCompiler.ICompiler) =
   let block =
      match mb.GetCustomAttribute<JSEmitAttribute>() with
      | meth when meth <> Unchecked.defaultof<_> ->
         let code =
            vars 
            |> List.mapi (fun i v -> i,v)
            |> List.fold (fun (acc:string) (i,v) ->
               acc.Replace(sprintf "{%i}" i, v.Name)
               ) meth.Emit
         EmitBlock code
      | _ -> Block(compiler.Compile ReturnStrategies.returnFrom bodyExpr)
   Lambda(vars, block)


let preCompile expr compiler =
   let usedMethods = findUsedMethods expr compiler

   let methodNames = 
      usedMethods 
      |> Seq.map (fun (KeyValue(name,_)) -> Var.Global(name, typeof<obj>)) 
      |> Seq.toList

   let declarations = 
      match methodNames with
      | [] -> []
      | _ -> [Declare methodNames]

   let assignments =
      usedMethods |> Seq.map (fun (KeyValue(name, (mi, vars, bodyExpr))) ->
         let methodExpr = genMethod mi vars bodyExpr compiler
         Assign(Reference (Var.Global(name, typeof<obj>)), methodExpr))
      |> Seq.toList

   List.append declarations assignments

let components = [ 
   methodCalling
   propertyGetting
   propertySetting
   constructingInstances
   fieldGetting
   fieldSetting
]