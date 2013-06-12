module (*internal*) FunScript.Objects

open AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open System

let rec private isAvailable (compiler : InternalCompiler.ICompiler) (mb : MethodBase) =
      mb <> null && (
         let t = mb.DeclaringType
         FSharpType.IsUnion(t, BindingFlags.Public ||| BindingFlags.NonPublic)  || 
         FSharpType.IsRecord(t, BindingFlags.Public ||| BindingFlags.NonPublic) || 
         Expr.TryGetReflectedDefinition(mb).IsSome ||
         compiler.ReplacementFor mb Quote.CallType.MethodCall |> Option.exists (isAvailable compiler))

let private propertyGetter =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.PropertyGet(Some(Split(objDecl, objRef)), pi, [])
            when isAvailable compiler (pi.GetGetMethod(true))
            ->
         [ yield! objDecl 
           yield returnStategy.Return <| PropertyGet(objRef, JavaScriptNameMapper.sanitizeAux pi.Name)
         ]
      | _ -> []

let private propertySetter =
   CompilerComponent.create <| fun (|Split|) compiler returnStategy ->
      function
      | Patterns.PropertySet(Some(Split(objDecl, objRef)), pi, [], Split(valDecl, valRef))
            when isAvailable compiler (pi.GetSetMethod(true))
            ->
         [ yield! objDecl 
           yield! valDecl
           yield Assign(PropertyGet(objRef, JavaScriptNameMapper.sanitizeAux pi.Name), valRef)
           if returnStategy = ReturnStrategies.inplace then
               yield returnStategy.Return Null 
         ]
      | _ -> []


let localized (name:string) =
   let sections = name.Split '-'
   JavaScriptNameMapper.sanitize name sections.[sections.Length - 1]

let private getAllMethods (t:System.Type) =
   t.GetMethods(
      BindingFlags.Public ||| 
      BindingFlags.NonPublic ||| 
      BindingFlags.FlattenHierarchy ||| 
      BindingFlags.Instance)

let replaceIfAvailable (compiler:InternalCompiler.ICompiler) (mb : MethodBase) callType =
   match compiler.ReplacementFor mb callType with
   | None -> mb //GetGenericMethod()...
   | Some mi -> upcast mi

let methodCallPattern (mb:MethodBase) =
   let argCounts = mb.GetCustomAttribute<CompilationArgumentCountsAttribute>()
   match Expr.TryGetReflectedDefinition mb with
   | Some (DerivedPatterns.Lambdas(vars, bodyExpr)) 
      when argCounts <> Unchecked.defaultof<_>
           && mb.IsStatic ->
      let argCounts = argCounts.Counts |> Seq.toList
      let varsAndConstructors =
         List.map2 (fun (vars:Var list) count ->
            if vars.Length = count then
               vars, None
            elif vars.Length = 1 then
               let var = vars.Head
               let genericArgs = var.Type.GetGenericArguments()
               let subVars =
                  [  for i = 0 to count - 1 do
                        yield Var(sprintf "%s_%i" var.Name i, genericArgs.[i], var.IsMutable)
                  ]
               let refs = subVars |> List.map Expr.Var
               let construction = if refs = [] then Expr.Value( () ) else Expr.NewTuple(refs)
               subVars, Some (var, construction) 
            else failwith "Unexpected argument format"               
            ) vars argCounts
      let vars  = varsAndConstructors |> List.collect fst
      let constructions = varsAndConstructors |> List.choose snd
      let bodyExpr =
         constructions |> List.fold (fun acc (var, value) ->
            Expr.Let(var, value, acc)) bodyExpr
      Some(vars, bodyExpr)
   | Some (DerivedPatterns.Lambdas(vars, bodyExpr)) ->
      Some(vars |> List.concat, bodyExpr)
   | Some expr ->
      Some([], expr)
   | None -> None

let (|CallPattern|_|) = methodCallPattern

let getFields (t:Type) =
   t.GetProperties(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
   |> Seq.map (fun p -> p, p.GetCustomAttribute<CompilationMappingAttribute>())
   |> Seq.filter (fun (p, attr) -> not <| obj.ReferenceEquals(null, attr)) 
   |> Seq.filter (fun (p, attr) -> SourceConstructFlags.Field = attr.SourceConstructFlags)
   |> Seq.sortBy (fun (p, attr) -> attr.SequenceNumber)
   |> Seq.map (fun (p, attr) -> JavaScriptNameMapper.sanitizeAux p.Name, p.PropertyType)
   |> Seq.toList

let components = [ 
   propertyGetter
   propertySetter
]