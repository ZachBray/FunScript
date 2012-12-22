module internal FunJS.TypeScript.TypeGenerator

open FunJS.TypeScript.AST
open Samples.FSharp.ProvidedTypes
open System
open System.Reflection
open Microsoft.FSharp.Quotations

type private MergedType =
 { TypePath: string list
   Members: TSObjectMember list
   HasInterface: bool
   SuperTypes: string list list }

let private createTypeNamed typePath = 
   {  
      TypePath = typePath
      Members = []
      HasInterface = false
      SuperTypes = []
   }
   
let private genEnumMember enumName flagName =
   let var = 
      { 
         TSVariable.Name = flagName
         IsOptional = false
         Type = Interface enumName 
      }
   Property(var, true)
   
let private obtainType name types =
   match types |> Map.tryFind name with
   | Some t -> t
   | None -> createTypeNamed name
   
// TODO: Add support for these
// {
let private isStructural = function
   | Structural _ -> true
   | _ -> false
   
let private isUnsupportedMember = function
   | Property(v,_) -> isStructural v.Type
   | Method(f,_) -> 
      isStructural f.Type || 
      f.Parameters |> List.exists (fun p -> 
         isStructural p.Var.Type)
   | Indexer _ -> true
// }
                     
let private addMembers typePath members hasInterface types =
   let supportedMembers = 
      members |> List.filter (not << isUnsupportedMember)
   let updatedType =
      let original = types |> obtainType typePath
      { original with
            Members = original.Members @ supportedMembers
            HasInterface = original.HasInterface || hasInterface
      }
   types |> Map.add typePath updatedType
  
let private extractTypePath (typePathStr:string) =
   typePathStr.Split([|'.'|]) |> Array.toList |> List.rev
  
let private addSuperTypes typePath superTypes types =
   let updatedType =
      let original = types |> obtainType typePath
      let superTypes = superTypes |> List.map extractTypePath
      { original with SuperTypes = superTypes @ original.SuperTypes}
   types |> Map.add typePath updatedType

let rec private addGlobal typePath glob =
   match glob with
   | TSGlobal.DeclareVar v ->
      addMembers typePath [Property(v, true)] false
   | TSGlobal.DeclareFunction f ->
      addMembers typePath [Method(f, true)] false
   | TSGlobal.DeclareClass obj
   | TSGlobal.DeclareObject obj ->   
      addMembers (obj.Name :: typePath) obj.Members false
      >> addSuperTypes typePath obj.SuperTypes
   | TSGlobal.DeclareInterface obj ->
      let typePath = obj.Name :: typePath
      addMembers typePath obj.Members true
      >> addSuperTypes typePath obj.SuperTypes
   | TSGlobal.DeclareEnum(name, flags) ->
      let members = flags |> List.map (genEnumMember name)
      let typePath = name :: typePath
      addMembers typePath members false
   | TSGlobal.DeclareModule(name, globals) ->
      let typePath = extractTypePath name @ typePath
      List.foldBack (addGlobal typePath) globals
         
let private mergeTypes globals =
   List.foldBack (addGlobal []) globals Map.empty
   
let rec private obtainTypeDef typePath defs =
   // TODO: lenient searches from inner modules?
   match !defs |> Map.tryFind typePath with
   | Some t -> t : ProvidedTypeDefinition
   | None ->
      let typeName = typePath |> List.head
      let baseType = typePath |> List.tail
      let parent = obtainTypeDef baseType defs
      let typeDef = ProvidedTypeDefinition(className=typeName, baseType=Some typeof<obj>)
      parent.AddMember(typeDef)
      defs := !defs |> Map.add typePath typeDef
      typeDef

let private isConstructor (f:TSFunction) =
   match f.Name with
   | "new" | "constructor" -> true
   | _ -> false

let private partitionMembers members =
   members |> List.fold (fun (ctors, meths, props) -> 
      function
      | Method (f, isStatic) ->
         if not isStatic && isConstructor f then f::ctors, meths, props
         else ctors, (f, isStatic)::meths, props
      | Property (v, isStatic) -> ctors, meths, (v, isStatic)::props
      | Indexer _ -> (* TODO *) ctors, meths, props) ([], [], [])

let rec private findSubTypePermutations subTypeLookup tsType =
   match tsType with
   | Interface name ->
      let typePath = extractTypePath name
      match subTypeLookup |> Map.tryFind typePath with
      | None -> [tsType]
      | Some subTypes ->
         tsType :: (subTypes |> List.map (String.concat "." >> Interface))
   | Lambda(parameters, returnType) ->
      let rec permutations parameters =
         seq {
            match parameters with
            | [] -> yield []
            | (p:TSParameter)::ps ->
               let pSubTypes = 
                  findSubTypePermutations subTypeLookup p.Var.Type
               let pSubTypeMappings =
                  pSubTypes |> List.map (fun pSubType ->
                     { p with Var = { p.Var with Type = pSubType  } })
               for combination in permutations ps do
                  for pSubTypeMapping in parameters do
                     yield pSubTypeMapping::combination
         } 
      let paramPermutations = permutations parameters
      let returnSubTypes = findSubTypePermutations subTypeLookup returnType
      paramPermutations |> Seq.collect (fun permutation ->
         returnSubTypes |> List.map (fun returnSubType ->
            Lambda(permutation, returnSubType)
         )
      ) |> Seq.toList    
   | t -> [t]
   

let rec private createParameterPermutations subTypeLookup parameters =
   seq {
      match parameters with
      | [] -> yield []
      | (p:TSParameter)::ps ->
         for ps in createParameterPermutations subTypeLookup ps do
            if p.Var.IsOptional then yield ps
            let paramType = p.Var.Type
            let paramSubTypes = findSubTypePermutations subTypeLookup paramType
            for paramSubType in paramSubTypes do
               let subP = { p with Var = { p.Var with Type = paramSubType } }
               yield subP::ps
   }
   
let private makeTupleType ts =
   let tupleType =
      match ts |> List.length with
      | 2 -> typedefof<_ * _>
      | 3 -> typedefof<_ * _ * _>
      | 4 -> typedefof<_ * _ * _ * _>
      | 5 -> typedefof<_ * _ * _ * _ * _>
      | 6 -> typedefof<_ * _ * _ * _ * _ * _>
      | 7 -> typedefof<_ * _ * _ * _ * _ * _ * _>
      | _ -> failwith "Unsupported tuple size"
   ProvidedSymbolType(Generic tupleType, ts) :> Type

let rec private convertToTypeDef defs tsType =
   match tsType with
   | Any -> typeof<obj>
   | Boolean -> typeof<bool>
   | String -> typeof<string>
   | Number -> typeof<float>
   | Void -> typeof<unit>
   | TSType.Array (TSType.Array t) ->
      let actualT: System.Type = convertToTypeDef defs t
      try actualT.MakeArrayType(2)
      with _ -> failwithf "Could not make array from Type: %A" t
   | TSType.Array t -> 
      let actualT: System.Type = convertToTypeDef defs t
      try actualT.MakeArrayType()
      with _ -> failwithf "Could not make array from Type: %A" t
   //TODO: Param arrays
   | Lambda([], retT) ->
      //let domain = typeof<unit>
      let range = convertToTypeDef defs retT
      // NOTE:
      // You would think that it would be ok to do this here:
      //    FSharpType.MakeFunctionType(domain, range)
      // It isn't because if the domain/range includes a ProvidedType
      // (which is _not_ a runtime type) it creates a TypeBuilderInstance
      // rather than a runtime type. TypeBuilderInstance does not implement
      // IsAssignableFrom. This means that calls to the method that takes
      // or returns the lambda fail the checkArgs test when creating a Call 
      // quotation Expr.
      let genericType = typedefof<_ -> _>
      ProvidedSymbolType(SymbolKind.Generic genericType, [typeof<unit>; range]) :> Type
   | Lambda([p], retT) ->
      let domain = convertToTypeDef defs p.Var.Type
      let range = convertToTypeDef defs retT
      let genericType = typedefof<_ -> _>
      ProvidedSymbolType(SymbolKind.Generic genericType, [domain; range]) :> Type
   | Lambda(ps, retT) ->
      try
         let domain = 
            ps 
            |> List.map (fun p -> convertToTypeDef defs p.Var.Type)
            |> makeTupleType
         let range = convertToTypeDef defs retT
         let genericType = typedefof<_ -> _>
         ProvidedSymbolType(SymbolKind.Generic genericType, [domain; range]) :> Type
      with ex ->
         failwithf "Failed to make function type. %s For: %A => %A" ex.Message ps retT
   | Structural obj -> 
      // TODO: Should we replace these up front with interfaces?
      //       Or emit them on demand?
      failwith "TODO"
   | Class name
   | Interface name
   | Enumeration name 
   | GlobalObject name ->
      let typePath = extractTypePath name
      defs |> obtainTypeDef typePath :> Type

let private createParameter defs (param:TSParameter) =
   let paramType = convertToTypeDef defs param.Var.Type
   ProvidedParameter(name=param.Var.Name, parameterType=paramType)

type CallType = Static | Instance

let private exprFor mi callType methName exprs =
   let methNameExpr = Expr.Value methName
   let isStaticExpr = Expr.Value ((callType = Static))
   let boxedExprs = exprs |> List.map (fun e -> Expr.Coerce(e, typeof<obj>))
   let amendedExprs = [isStaticExpr; methNameExpr; Expr.NewArray(typeof<obj>, boxedExprs)]
   try
      Expr.Coerce(Expr.Call(mi, amendedExprs), typeof<obj>)
   with ex ->
      let expectedCount = mi.GetParameters().Length
      let actualCount = amendedExprs.Length
      let msg = 
         sprintf "Expected %i parameters but got %i.\n%A\n%s" 
            expectedCount actualCount exprs
            ex.Message
      raise(Exception(msg, ex))

let private call callType methName exprs = exprFor Emit.Call callType methName exprs
let private createNew methName exprs = exprFor Emit.New Static methName exprs
let private replaceMethod methName exprs = exprFor Emit.ReplaceMethod Instance methName exprs
let private propertyGet callType methName exprs = exprFor Emit.PropertyGet callType methName exprs
let private propertySet callType methName exprs = exprFor Emit.PropertySet callType methName exprs
let private createObj() = exprFor Emit.CreateObject Static "boo" []

let private pathToName = List.rev >> String.concat "."

let private createConstructors defs subTypeLookup (ctors:TSFunction list) parentType parentTypePath =
   ctors 
   |> Seq.collect (fun ctor -> createParameterPermutations subTypeLookup ctor.Parameters)
   |> Seq.distinctBy (fun paramReqs -> paramReqs |> List.map (fun p -> p.Var.Type))
   |> Seq.map (List.map (createParameter defs))
   |> Seq.map (fun providedParams ->
      ProvidedMethod(
         methodName="CreateInstance",
         parameters=providedParams,
         returnType=parentType,
         IsStaticMethod=true,
         InvokeCode=fun exprs -> createNew (pathToName parentTypePath) exprs))
   |> Seq.toList
         
let sanitise = function
   | "" -> "Invoke" 
   | name -> name 
   
let private createMethods defs subTypeLookup name (meths:(TSFunction * bool) list) (parent:MergedType) =
   let isStatic = meths |> List.exists snd
   let meths = 
      meths |> List.choose (fun (m, isS) ->
         if isS = isStatic then Some m
         else None)
   let returnTSType = 
      let firstMeth = meths |> List.head
      let allSame = 
         meths |> List.forall (fun m -> 
            m.Type = firstMeth.Type)
      if allSame then firstMeth.Type
      else Any
   let returnType = convertToTypeDef defs returnTSType
   meths 
   |> Seq.collect (fun meth -> createParameterPermutations subTypeLookup meth.Parameters)
   |> Seq.distinctBy (fun paramReqs -> paramReqs |> List.map (fun p -> p.Var.Type))
   |> Seq.collect (fun paramReqs ->
      seq {
         let providedParams = paramReqs |> List.map (createParameter defs)
         yield ProvidedMethod(
            methodName=sanitise name,
            parameters=providedParams,
            returnType=returnType,
            IsStaticMethod=isStatic,
            InvokeCode=fun exprs ->
               if isStatic then
                  call Static (pathToName (name::parent.TypePath)) exprs
               else call Instance name exprs)
         if parent.HasInterface && 
            not isStatic &&
            paramReqs.Length < 8 then
            let parameter =
               let parameterType = Lambda(paramReqs, returnTSType)
               { Var = { Name = "replacement"; IsOptional = false; Type = parameterType }
                 IsParamArray = false }
            yield ProvidedMethod(
               methodName="set_" + sanitise name,
               parameters=[createParameter defs parameter],
               returnType=typeof<Void>,
               IsStaticMethod=false,
               InvokeCode=fun exprs -> replaceMethod name exprs)
      })
         
let private createProperty defs parentTypePath (prop:TSVariable, isStatic) =
   let propType = convertToTypeDef defs prop.Type
   ProvidedProperty(
      propertyName=prop.Name, 
      propertyType=propType, 
      IsStatic=isStatic,
      GetterCode=(fun exprs -> 
         if isStatic then
            propertyGet Static (pathToName (prop.Name::parentTypePath)) exprs
         else propertyGet Instance prop.Name exprs),
      SetterCode=(fun exprs -> 
         if isStatic then
            propertySet Static (pathToName (prop.Name::parentTypePath)) exprs
         else propertySet Instance prop.Name exprs))
      
let private createMembers defs subTypeLookup members (mergedType:MergedType) parentType =
   let ctors, meths, props = partitionMembers members
   let ctorDefs = createConstructors defs subTypeLookup ctors parentType mergedType.TypePath
   let methDefs = 
      let methsByName = meths |> Seq.groupBy (fun (m,_) -> m.Name)
      methsByName |> Seq.collect (fun (name, meths) ->
         createMethods defs subTypeLookup name (meths |> Seq.toList) mergedType)
      |> Seq.toList
   let propDefs = props |> List.map (createProperty defs mergedType.TypePath)
   ctorDefs, methDefs, propDefs

let private createType (mergedType:MergedType) types defs subTypeLookup =
   let typeDef = defs |> obtainTypeDef mergedType.TypePath
   let inheritedMembers = 
      mergedType.SuperTypes |> List.choose (fun typePath ->
         types |> Map.tryFind typePath)
      |> List.collect (fun (mergedType:MergedType) -> mergedType.Members)
   let allMembers = inheritedMembers @ mergedType.Members
   let ctorDefs, methDefs, propDefs = 
      createMembers defs subTypeLookup allMembers mergedType typeDef
   // TODO: Would be nice to build a builder here or something
   // that makes it easier to use these interfaces.
   if mergedType.HasInterface then
      ProvidedConstructor(
         parameters=[], 
         InvokeCode=fun args -> createObj())
      |> typeDef.AddMember
   typeDef.AddMembers ctorDefs
   typeDef.AddMembers methDefs
   typeDef.AddMembers propDefs

let private buildSubTypeLookup mergedTypes =
   mergedTypes 
   |> Map.fold (fun acc subTypePath (mergedType:MergedType) ->
      mergedType.SuperTypes |> List.fold (fun acc superType ->
         match acc |> Map.tryFind superType with
         | Some subTypes -> acc |> Map.add superType (subTypePath :: subTypes)
         | None -> acc |> Map.add superType [subTypePath]) acc) Map.empty

let rec private mostSpecificPath mergedTypes parentTypePath path =
   match parentTypePath with
   | [] -> path
   | _::rest ->
      let combinedPath = path @ parentTypePath
      match mergedTypes |> Map.tryFind combinedPath with
      | None -> mostSpecificPath mergedTypes rest path
      | Some _ -> combinedPath

let rec private mostSpecificType mergedTypes parentTypePath = function
   | Interface name -> 
      let typePath = mostSpecificPath mergedTypes parentTypePath (extractTypePath name)
      let specificName = typePath |> List.rev |> String.concat "."
      Interface specificName
   | Lambda(parameters, returnType) ->
      let updatedParameters =
         parameters |> List.map (improveParameterTypePath mergedTypes parentTypePath)
      let updatedReturnType =
         mostSpecificType mergedTypes parentTypePath returnType 
      Lambda(updatedParameters, updatedReturnType)
   | t -> t

and private improveParameterTypePath mergedTypes parentTypePath (parameter:TSParameter) =
   let updatedType = mostSpecificType mergedTypes parentTypePath parameter.Var.Type
   { parameter with 
      Var = { parameter.Var with Type = updatedType } }

let private improveMethodTypePaths mergedTypes parentTypePath = function
   | Method(f, isStatic) ->
      let updatedF = 
         { f with 
            Type = mostSpecificType mergedTypes parentTypePath f.Type
            Parameters = f.Parameters |> List.map (improveParameterTypePath mergedTypes parentTypePath)  }
      Method(updatedF, isStatic)
   | Property(v, isStatic) ->
      let updatedV = 
         { v with 
            Type = mostSpecificType mergedTypes parentTypePath v.Type }
      Property(updatedV, isStatic)
   | unsupported -> unsupported

let private improveTypePaths mergedTypes =
   mergedTypes 
   |> Map.map (fun parentTypePath mergedType ->
      { mergedType with 
         MergedType.Members =  
            mergedType.Members 
            |> List.map (improveMethodTypePaths mergedTypes parentTypePath) 
      })

let generate globals root =
   let mergedTypes = mergeTypes globals |> improveTypePaths
   let subTypeLookup = buildSubTypeLookup mergedTypes
   let defs = ref(Map [[], root])
   for KeyValue(_, t) in mergedTypes do 
      createType t mergedTypes defs subTypeLookup