module internal FunScript.TypeScript.TypeGenerator

open System.Text
open System.Collections.Generic
open Microsoft.FSharp.Core.Printf
open FunScript.TypeScript.AST 

type Staticness =
    | Static
    | NonStatic

type TypeKind =
    | Interface
    | Class

type MemberKind =
    | Method of TypeParameter list * Type list * TypeAnnotation option
    | Property of TypeAnnotation option

let (|Staticness|) = function   
    | true -> Static
    | false -> NonStatic

type ReferencePosition =
    | UsedInConstraint
    | UsedInDeclaration

type TypeUsageContext = {
    UsagePosition : ReferencePosition
    EntityName : EntityName
    TypeParameters : TypeParameter list
}


let keywords =
    Set [|"abstract"; "as"; "base"; "bool"; "break"; "byte"; "case"; "catch"; "char";
    "checked"; "class"; "const"; "continue"; "decimal"; "default"; "delegate";
    "do"; "double"; "else"; "enum"; "event"; "explicit"; "extern"; "false";
    "finally"; "fixed"; "float"; "for"; "foreach"; "goto"; "if"; "implicit";
    "in"; "int"; "interface"; "internal"; "is"; "lock"; "long"; "namespace";
    "new"; "null"; "object"; "operator"; "out"; "override"; "params";
    "private"; "protected"; "public"; "readonly"; "ref"; "return"; "sbyte";
    "sealed"; "short"; "sizeof"; "stackalloc"; "static"; "string"; "struct";
    "switch"; "this"; "throw"; "true"; "try"; "typeof"; "uint"; "ulong";
    "unchecked"; "unsafe"; "ushort"; "using"; "virtual"; "void"; "volatile";
    "while"|]

let buildCode(definitionFiles) =
    let auxCount = ref 0

    let emitted = HashSet()
    let code = StringBuilder()
    
    let declaredInterfaces = Dictionary()
    let awaitingDeclaration = Dictionary()
    let anonymous = Dictionary()

    let codef fmt = kprintf (code.AppendLine >> ignore) fmt

    let sanitise str =
        [   " ", "_"
            "$", "Dollar"
            "-", "_"
            ".", "_"
        ]
        |> List.fold (fun (str : string) (x, y) -> str.Replace(x, y)) str
        |> fun str -> 
            if keywords.Contains str || str = "" then "_" + str 
            else
                let almostSane =
                    str |> String.collect(function
                        | '_' -> "_"
                        | c when c >= 'a' && c <= 'z' -> c.ToString()
                        | c when c >= 'A' && c <= 'Z' -> c.ToString()
                        | c when c >= '0' && c <= '9' -> c.ToString()
                        | c -> sprintf "CHAR%i" (int c))
                let firstC = almostSane.[0] 
                if firstC >= '0' && firstC <= '9' then "_" + almostSane
                else almostSane
        

    let stringify(str : string) =
        str.Replace("\"", "\\\"")

    let buildTypeNameFromEntityName (EntityName path) = 
        match path with
        | [] -> failwith "Expected something"
        | _ -> path |> List.map sanitise |> String.concat "."

    let buildNamespaceFromEntityName (EntityName path) =
        match path with
        | [] -> None, None
        | _ -> 
            let csNs = path |> List.map sanitise |> String.concat "."
            let jsNs = path |> String.concat "."
            Some csNs, Some jsNs

    let buildPredefinedTypeReference = function
        | Any -> "object"
        | Number -> "double"
        | Boolean -> "bool"
        | String -> "string"
        | Void -> "Microsoft.FSharp.Core.Unit"

    let rec buildReferenceToTypeReference usageContext (TypeReference(en, ta)) =
        let entityName = buildTypeNameFromEntityName en
        match ta with
        | [] ->
            match entityName with
            | "Array" -> "object[]"
            | _ -> entityName
        | tas -> 
            let args = 
                tas |> List.map (buildReferenceToType usageContext)
                |> String.concat ", "
            sprintf "%s<%s>" entityName args

    and buildReferenceToType usageContext = function
        | Predefined pt -> buildPredefinedTypeReference pt
        | Reference tr -> buildReferenceToTypeReference usageContext tr
        | Literal tl -> buildReferenceToTypeLiteral usageContext tl
        | Query _ -> buildPredefinedTypeReference Any

    and buildReferenceToTypeLiteral (usageContext : TypeUsageContext) = function
        | LiteralObjectType objType -> 
            incr auxCount
            let id = !auxCount
            let name = sprintf "IAnonymousType%i" id
            let heritage = ClassHeritage(None, [])
            buildObjectType Class usageContext.EntityName name usageContext.TypeParameters heritage objType
            let tpStr, _ =
                buildTypeParameters usageContext.EntityName usageContext.TypeParameters usageContext.TypeParameters
            let csNs, _ = buildNamespaceFromEntityName usageContext.EntityName
            let typeName =
                match csNs with
                | None -> sprintf "%s%s" name tpStr
                | Some ns -> sprintf "%s.%s%s" ns name tpStr
            let (EntityName path) = usageContext.EntityName
            let args = 
                usageContext.TypeParameters |> List.map (fun (TypeParameter(id, _)) ->
                    TypeArgument.Reference(TypeReference(EntityName [id], [])))
            let t = Reference(TypeReference(EntityName(path @ [name]), args))
            anonymous.[typeName] <- t
            typeName
        | LiteralArrayType t -> 
            match usageContext.UsagePosition with
            | UsedInDeclaration _ ->
                sprintf "%s[]" (buildReferenceToType usageContext t)
            | UsedInConstraint ->
                //Note: this is because of C#'s limited generic constraints
                sprintf "System.Collections.Generic.IList<%s>" (buildReferenceToType usageContext t)
        | LiteralFunctionType(tps, ps, t) 
        | LiteralConstructorType(tps, ps, t)->
            match tps with
            | [] -> buildLambdaType usageContext ps t
            | _ -> buildReferenceToType usageContext (Predefined Any)

    and buildLambdaType usageContext (ParameterList(req, opt, rest)) t =
        match req, opt, rest with
        | req, opt, None -> 
            let reqs = req |> Seq.map (function
                | StringEnumParameter _ -> 
                    buildReferenceToType usageContext (Predefined String)
                | VariableParameter(_, _, ta) -> 
                    buildReferenceToTypeAnnotation usageContext.EntityName usageContext.TypeParameters ta)
            let opts = opt |> Seq.map (function
                | DefaultParameter(_, _, ta, _)
                | OptionalParameter(_, _, ta) -> 
                    buildReferenceToTypeAnnotation usageContext.EntityName usageContext.TypeParameters ta)
            let rt = buildReferenceToType usageContext t
            let argTs = Seq.append reqs opts |> Seq.toList
            let argTsStr =
                match argTs with
                | [] -> "Microsoft.FSharp.Core.Unit"
                | _ -> argTs |> String.concat ", "
            sprintf "global::System.Func<%s, %s>" argTsStr rt
        | _ -> buildReferenceToType usageContext (Predefined Any)

    and buildReferenceToTypeAnnotation entityName tps typeAnnotation =
        let usageContext = {
            UsagePosition = UsedInDeclaration
            EntityName = entityName
            TypeParameters = tps
        }
        match typeAnnotation with
        | None -> buildReferenceToType usageContext (Predefined Any)
        | Some (TypeAnnotation t) -> buildReferenceToType usageContext t

    and buildReferenceToTypeParameter entityName tps (TypeParameter(id, t)) =
        match t with
        | None -> id, []
        | Some t ->
            let usageContext = {
                UsagePosition = UsedInConstraint
                EntityName = entityName
                TypeParameters = tps
            }
            let baseTypeName = buildReferenceToType usageContext t
            id, [sprintf "%s : %s" id baseTypeName]

    and buildTypeParameters entityName classTps tps =
        let typeParams, typeConstraints =
            tps |> List.map (buildReferenceToTypeParameter entityName classTps)
            |> List.unzip
        let typeContraintClause =
            match typeConstraints |> List.concat with
            | [] -> ""
            | tcs -> "\n          where " + (tcs |> String.concat "\n           where ")
        let typeParamStr =
            match typeParams with
            | [] -> ""
            | tps -> sprintf "<%s>" (tps |> String.concat ", ")
        typeParamStr, typeContraintClause

    and buildRequiredParameter entityName classTps = function
        | VariableParameter(_, id, typeAnnotation) ->
            let returnTypeName = buildReferenceToTypeAnnotation entityName classTps typeAnnotation
            fun i -> "", Some(sprintf "%s %s" returnTypeName (sanitise id)), sprintf "{%i}" i
        | StringEnumParameter(id, value) ->
            fun _ -> sanitise value, None, sprintf "\"%s\"" value

    and buildOptionalParameter entityName classTps = function
        //TODO: Support all versions/default values
        | OptionalParameter(_, id, typeAnnotation)
        | DefaultParameter(_, id, typeAnnotation, _) ->
            let returnTypeName = buildReferenceToTypeAnnotation entityName classTps typeAnnotation
            fun i -> "", Some(sprintf "%s %s" returnTypeName (sanitise id)), sprintf "{%i}" i

    and buildRestParameter entityName classTps (RestParameter(id, typeAnnotation)) =
        let returnTypeName = buildReferenceToTypeAnnotation entityName classTps typeAnnotation
        // Note: this is a bit dodgy. Could be made less strIngly typed.
        let adjustedReturnTypeName =
            if returnTypeName.EndsWith "[]" then returnTypeName
            elif returnTypeName.StartsWith "Array<" && returnTypeName.EndsWith ">" then
                let startLength = "Array<".Length
                let endLength = ">".Length
                returnTypeName.Substring(startLength, returnTypeName.Length - startLength - endLength) + "[]"
            else "object[]"
        fun i -> "", Some(sprintf "params %s %s" adjustedReturnTypeName (sanitise id)), sprintf "{%i...}" i

    and buildParameters entityName classTps staticness (ParameterList(reqs, opts, rest)) =
        let paramShift = if staticness = Static then 0 else 1
        let specializations, parameters, emits = 
            seq {
                yield! reqs |> Seq.map (buildRequiredParameter entityName classTps)
                yield! opts |> Seq.map (buildOptionalParameter entityName classTps)
                yield! rest |> Option.toList |> List.map (buildRestParameter entityName classTps)
            } |> Seq.mapi (fun i f -> f (i + paramShift))
            |> Seq.toArray
            |> Array.unzip3
        specializations |> String.concat "",
        sprintf "(%s)" (parameters |> Seq.choose id |> String.concat ", "),
        sprintf "(%s)" (emits |> String.concat ", ")

    /// Removes any reference to object literals and instead replaces it with a type reference.
    and solidifyReturnType typeAnnotation typeFullName =
        match typeAnnotation with
        | None -> None
        | Some _ -> 
            match anonymous.TryGetValue typeFullName with
            | false, _ -> typeAnnotation
            | true, t -> Some(TypeAnnotation t)

    and buildReferenceToCallSignatureAndReturnType entityName classTps staticness typeKind methodName (CallSignature(tps, ps, typeAnnotation)) =
        let typeFullName = buildReferenceToTypeAnnotation entityName classTps typeAnnotation
        let typeParamStr, typeConstraintClause = buildTypeParameters entityName classTps tps
        let specialization, parameters, emitCall = buildParameters entityName (classTps @ tps) staticness ps
        let impl = if typeKind = Interface then ";" else " { throw new global::System.Exception(); }"
        let returnType = solidifyReturnType typeAnnotation typeFullName
        sprintf "%s %s%s%s%s %s%s"
            typeFullName (sanitise methodName) specialization typeParamStr parameters typeConstraintClause impl,
        emitCall,
        returnType

    and buildReferenceToCallSignature entityName classTps staticness typeKind methodName callSig =
        let signature, emit, _ = 
            buildReferenceToCallSignatureAndReturnType 
                entityName classTps staticness typeKind methodName callSig
        signature, emit

    and buildGenericDeclaration typeKind
                         namespaceName className (typeParams, genericConstraints) heritageStr
                         signatures =
        let typeName, accessModifier =
            match typeKind with
            | Class -> "partial class", "public "
            | Interface -> "partial interface", ""
        let memberSignatures =
            signatures |> Seq.filter (fun (_, _, signature : string) ->
                let sigKey = signature.Replace("params", "").Replace(" ", "")
                let key = namespaceName, className, typeParams, sigKey
                emitted.Add key)
            |> Seq.map (fun (emitCode, staticness, signature) ->
                let staticness = if staticness = Static then "static " else ""
                sprintf """        [FunScript.JSEmitInline("%s")]
        %s%s%s
"""                 (stringify emitCode) accessModifier staticness signature)
            |> String.concat ""


        match namespaceName with
        | None ->
            codef """
public %s %s%s %s %s
{
%s
}
"""             typeName (sanitise className) typeParams heritageStr genericConstraints 
                memberSignatures
        | Some namespaceName ->
            codef """
namespace %s {
    public %s %s%s %s %s
    {
%s
    }
}"""            namespaceName typeName (sanitise className) typeParams heritageStr genericConstraints
                memberSignatures


    and buildDeclaration arg = buildGenericDeclaration Class arg
    and buildInterfaceDeclaration arg = buildGenericDeclaration Interface arg

//    // TODO: What about on interfaces? Do we need to maintain a mapping?
//    and tryClaim entityName className classTps typeKind memberName memberKind =
//        let numbered str i =
//            match i with
//            | 0 -> str
//            | _ -> sprintf "%s%i" str i
//        let rec tryClaimAux i j =
//            let className = numbered className i
//            let memberName = numbered memberName j
//            let key = entityName, className, classTps
//            match claims.TryGetValue key with
//            | false, _ -> 
//                let innerClaims = Dictionary()
//                innerClaims.Add(memberName, [memberKind]) 
//                claims.Add(key, (Some typeKind, innerClaims))
//                className, memberName
//            | true, (foundKind, innerClaims) ->
//                match foundKind with
//                | Some kind when kind = typeKind ->
//                    match innerClaims.TryGetValue memberName with
//                    | false, _ -> 
//                        innerClaims.Add(memberName, [memberKind])
//                        className, memberName
//                    | true, kinds ->
//                        match memberKind with
//                        | Property _ -> tryClaimAux i (j+1)
//                        | Method(tps, ps, t) ->
//                            let isInvalid = 
//                                kinds |> List.exists (function
//                                    | Property _ -> true
//                                    | x -> x = memberKind)
//                            if isInvalid then tryClaimAux i (j+1)
//                            else className, memberName
//                | Some _ | None -> tryClaimAux (i+1) 0
//        tryClaimAux 0 0

    and buildVariableDeclaration entityName classTps varName typeAnnotation =
        let nsCs, nsJs = buildNamespaceFromEntityName entityName
        let typeFullName = buildReferenceToTypeAnnotation entityName classTps typeAnnotation
        let emitCode = 
            match nsJs with 
            | None -> varName
            | Some ns -> sprintf "%s.%s" ns varName
        let signature = sprintf "%s %s { get; set; }" typeFullName (sanitise varName)
        buildDeclaration nsCs "Globals" ("","") "" [emitCode, Static, signature]

    and buildFunctionDeclaration entityName classTps name callSignature =
        let nsCs, nsJs = buildNamespaceFromEntityName entityName
        let signature, emitCall = 
            buildReferenceToCallSignature entityName classTps Static Class name callSignature
        let emitCode = 
            match nsJs with 
            | None -> sprintf "%s%s" name emitCall
            | Some ns -> sprintf "%s.%s%s" ns name emitCall
        buildDeclaration nsCs "Globals" ("","") "" [emitCode, Static, signature]

    and buildPropertyName = function
        | NameIdentifier id -> sanitise id, id
        | NameStringLiteral id -> sanitise id, sprintf "[\"%s\"]" id
        | NameNumericLiteral n -> sprintf "_%i" n, sprintf "[%i]" n 

    and buildPropertyDeclaration propMap entityName classTps staticness propName typeAnnotation =
        let _, nsJs = buildNamespaceFromEntityName entityName
        let nameCs, nameJs = buildPropertyName propName
        let typeFullName = buildReferenceToTypeAnnotation entityName classTps typeAnnotation
        let signature = sprintf "%s %s { get; set; }" typeFullName nameCs
        let emit = 
            match nsJs, staticness with
            | None, Static -> nameJs
            | Some nsJs, Static -> sprintf "%s.%s" nsJs nameJs
            | _, NonStatic -> sprintf "{0}.%s" nameJs
        let returnType = solidifyReturnType typeAnnotation typeFullName
        propMap |> Option.iter (fun propMap ->
            propMap := !propMap |> Map.add propName returnType)
        emit, staticness, signature

    and buildMethodDeclaration propMap entityName classTps staticness typeKind propName callSignature =
        let _, nsJs = buildNamespaceFromEntityName entityName
        let nameCs, nameJs = buildPropertyName propName
        let preEmit = 
            match nsJs, staticness with
            | None, Static -> ""
            | Some nsJs, Static -> sprintf "%s." nsJs
            | _, NonStatic -> "{0}."
        let signature, emitCall, returnType = 
            buildReferenceToCallSignatureAndReturnType entityName classTps staticness typeKind nameCs callSignature
        propMap |> Option.iter (fun propMap ->
            propMap := !propMap |> Map.add propName returnType)
        let emit = sprintf "%s%s%s" preEmit nameJs emitCall
        emit, staticness, signature

    and buildCallDeclaration entityName classTps staticness typeKind callSignature =
        let _, nsJs = buildNamespaceFromEntityName entityName
        let preEmit = 
            match nsJs, staticness with
            | None, Static -> ""
            | Some nsJs, Static -> nsJs
            | _, NonStatic -> "{0}"
        let signature, emitCall = 
            buildReferenceToCallSignature entityName classTps staticness typeKind "Invoke" callSignature
        preEmit + emitCall, staticness, signature

    and buildAmbientMemberDeclaration entityName classTps = function
        | AmbientPropertyDeclaration(_, Staticness staticness, propName, typeAnnotation) ->
            buildPropertyDeclaration None entityName classTps staticness propName typeAnnotation
        | AmbientMethodDeclaration(_, Staticness staticness, propName, callSignature) ->
            buildMethodDeclaration None entityName classTps staticness Class propName callSignature

    and buildIndexSignature entityName classTps typeKind indexSig =
        let indexType, typeAnnotation =
            match indexSig with
            | IndexSignatureNumber(_, typeAnnotation) -> "int", typeAnnotation
            | IndexSignatureString(_, typeAnnotation) -> "string", typeAnnotation
        let typeFullName = buildReferenceToTypeAnnotation entityName classTps (Some typeAnnotation)
        let impl =
            match typeKind with
            | Interface -> "{ get; set; }"
            | Class -> """
        {
            get { throw new global::System.Exception(); }
            set { throw new global::System.Exception(); }
        }"""
        let signature =
            sprintf "%s this[%s i] %s" typeFullName indexType impl
        let emit = "{0}[{1}]"
        emit, NonStatic, signature
             
    and buildConstructorDeclaration ((EntityName path) as entityName) className classTps tps ps ta =
        let _, nsJs = buildNamespaceFromEntityName entityName
        let classTas = classTps |> List.map (fun (TypeParameter(id, _)) -> Reference(TypeReference(EntityName [id], [])))
        let ta = defaultArg ta (TypeAnnotation(Reference(TypeReference(EntityName(path @ [className]), classTas))))
        let signature, emitCall = 
            buildReferenceToCallSignature entityName classTps Static Class "Create" (CallSignature(tps, ps, Some ta))
        let emit = 
            match nsJs with
            | None -> sprintf "%s%s" className emitCall
            | Some ns -> sprintf "%s.%s%s" ns className emitCall
        emit, Static, signature

    and buildClassBodyElement entityName className classTps = function
        | AmbientConstructorDeclaration ps ->
            buildConstructorDeclaration entityName className classTps [] ps None
        | AmbientMemberDeclaration memberDecl ->
            buildAmbientMemberDeclaration entityName classTps memberDecl
        | AmbientIndexSignature indexSignature ->
            buildIndexSignature entityName classTps Class indexSignature

    and buildClassHeritage entityName classTps (ClassHeritage(extendsClause, implementsClauses)) =
        let usageContext = {
            UsagePosition = UsedInDeclaration
            EntityName = entityName
            TypeParameters = classTps
        }
        let extends = 
            extendsClause |> Option.map (buildReferenceToTypeReference usageContext)
        let implements = 
            implementsClauses |> List.map (buildReferenceToTypeReference usageContext)
        let superTypes =
            match extends with
            | None -> implements
            | Some x -> x::implements
        let heritageStr =
            match superTypes with
            | [] -> ""
            | _ -> sprintf " : %s" (superTypes |> String.concat ", ")
        heritageStr

    and getInterfaceDeclarationKeys basePath ((EntityName supPath) as entityName) count =
        [
            yield entityName, count
            match basePath with
            | None -> ()
            | Some(EntityName basePath) -> 
                let parts xs =
                    let rec parts acc = function
                        | [] -> []::acc
                        | x::xs -> parts (xs::acc) xs
                    parts [xs |> List.rev] (xs |> List.rev) |> List.map List.rev
                for basePathPart in parts basePath do
                    yield EntityName(basePathPart @ supPath), count
        ] |> Seq.distinct

    and emptyMemberTable() = lazy Map.empty

    and buildMemberTable members = lazy members

    // TODO: It would be nice to do this without all the string hacks!
    and applyParamsToMemberTable interfaceTps classInterfaceTas (membersDelayed : _ Lazy) =
        lazy
            let members = membersDelayed.Value

            let tpsLookup =
                List.map2 (fun (TypeParameter(id, _)) ta ->
                    let search = Reference(TypeReference(EntityName [id], []))
                    search, ta) interfaceTps classInterfaceTas
                |> Map.ofList

            let rec mapType = function
                | (Predefined _ as preDef) -> preDef
                | (Reference tr as ref) ->
                    match tpsLookup.TryFind ref with
                    | None -> Reference(mapReference tr)
                    | Some rep -> rep
                | Literal l -> Literal(mapLiteral l)
                | Query _ -> Predefined Any

            and mapReference (TypeReference(entityName, tps)) =
                TypeReference(entityName, tps |> List.map mapType)

            and mapLiteral = function
                | LiteralObjectType(ObjectType members) ->
                    LiteralObjectType(ObjectType(members |> List.map mapMember))
                | LiteralArrayType t ->
                    LiteralArrayType(mapType t)
                | LiteralConstructorType(tps, ps, t) ->
                    LiteralConstructorType(tps, mapParameterList ps, mapType t)
                | LiteralFunctionType(tps, ps, t) ->
                    LiteralFunctionType(tps, mapParameterList ps, mapType t)

            and mapMember = function
                | MemberCallSignature callSig ->
                    MemberCallSignature(
                        mapCallSignature callSig)
                | MemberConstructSignature(ConstructSignature(tps, ps, ta)) ->
                    MemberConstructSignature(
                        ConstructSignature(mapCallParameters(tps, ps, ta)))
                | MemberIndexSignature indexSignature ->
                    MemberIndexSignature(
                        mapIndexSignature indexSignature)
                | MemberMethodSignature(MethodSignature(name, isOpt, callSig)) ->
                    MemberMethodSignature(
                        MethodSignature(name, isOpt, mapCallSignature callSig))
                | MemberPropertySignature(PropertySignature(name, isOpt, ta)) ->
                    MemberPropertySignature(
                        PropertySignature(name, isOpt, ta |> Option.map mapTypeAnnotation))
                
            and mapCallSignature(CallSignature(tps, ps, ta)) =
                CallSignature(mapCallParameters(tps, ps, ta))

            and mapCallParameters(tps, ps, ta) =
                tps, mapParameterList ps, ta |> Option.map mapTypeAnnotation

            and mapTypeAnnotation(TypeAnnotation t) =
                TypeAnnotation(mapType t)

            and mapIndexSignature = function
                | IndexSignatureString(id, ta) -> IndexSignatureString(id, mapTypeAnnotation ta)
                | IndexSignatureNumber(id, ta) -> IndexSignatureNumber(id, mapTypeAnnotation ta)

            and mapParameterList(ParameterList(reqs, opts, rest)) =
                ParameterList(
                    reqs |> List.map mapRequiredParameter,
                    opts |> List.map mapOptionalParameter,
                    rest |> Option.map mapRestParameter)

            and mapRequiredParameter = function
                | VariableParameter(acc, id, ta) -> 
                    VariableParameter(acc, id, ta |> Option.map mapTypeAnnotation)
                | x -> x

            and mapOptionalParameter = function
                | DefaultParameter(acc, id, ta, initVal) ->
                    DefaultParameter(acc, id, ta |> Option.map mapTypeAnnotation, initVal)
                | OptionalParameter(acc, id, ta) ->
                    OptionalParameter(acc, id, ta |> Option.map mapTypeAnnotation)

            and mapRestParameter(RestParameter(id, ta)) =
                RestParameter(id, ta |> Option.map mapTypeAnnotation)

            members |> Map.map (fun propName ta -> 
                ta |> Option.map (fun (TypeAnnotation t) -> 
                    TypeAnnotation(mapType t)))

    and mergeMemberTables (membersADelayed : _ Lazy) (membersBDelayed : _ Lazy) =
        lazy 
            let membersA = membersADelayed.Value
            let membersB = membersBDelayed.Value
            membersA |> Map.foldBack Map.add membersB

    and updateTriggers key f =
        let current =
            match awaitingDeclaration.TryGetValue key with
            | false, _ -> Map.empty
            | true, xs -> xs
        let next = f current
        if next |> Map.isEmpty then awaitingDeclaration.Remove key |> ignore
        else awaitingDeclaration.[key] <- next

    /// For topological declaration based on interface inheritance hierarchy.
    and awaitDeclaration ((_, listenerName) as listenerId) baseEntityName (ClassHeritage(_, implementsClauses)) f =
        let rec awaitDeclarationAux acc = function
            | [] -> f acc
            | TypeReference(entityName, args)::reqs ->
                let keys = getInterfaceDeclarationKeys (Some baseEntityName) entityName args.Length
                let triggerId = System.Guid.NewGuid()
                let hasExecuted = ref false
                let additionalTrigger = fun key ->
                    if not !hasExecuted then
                        hasExecuted := true
                        for key in keys do updateTriggers key (Map.remove triggerId)
                        let interfaceTps, members = declaredInterfaces.[key]
                        let parameterisedMembers = 
                            applyParamsToMemberTable interfaceTps args members
                        awaitDeclarationAux (mergeMemberTables parameterisedMembers acc) reqs
                let readyKey =
                    keys |> Seq.tryFind (declaredInterfaces.TryGetValue >> fst)
                if listenerName = "ICheckedObserver" then
                    printfn "..."
                match readyKey with
                | Some key -> additionalTrigger key
                | None ->
                    for key in keys do
                        updateTriggers key (Map.add triggerId (listenerId, additionalTrigger))
        awaitDeclarationAux (emptyMemberTable()) implementsClauses

    and notifyDeclaration (EntityName path) interfaceName interfaceTps members =
        let fullEntityName = EntityName(path @ [interfaceName])
        let key = getInterfaceDeclarationKeys None fullEntityName (List.length interfaceTps) |> Seq.head
        match declaredInterfaces.TryGetValue key with
        | false, _ ->
            declaredInterfaces.Add(key, (interfaceTps, buildMemberTable members))
            match awaitingDeclaration.TryGetValue key with
            | false, _ -> ()
            | true, triggers -> triggers |> Map.iter (fun _ (_, f) -> f key)
            awaitingDeclaration.Remove key |> ignore<bool>
        | true, (interfaceTps, memberTable) ->
            declaredInterfaces.[key] <- (interfaceTps, mergeMemberTables memberTable (buildMemberTable members))
            let name = buildTypeNameFromEntityName fullEntityName
            printfn "[WARN] Interface '%s' is split into multiple parts." name

    and buildClassDeclaration entityName name tps heritage bodyEls =
        let nsCs, nsJs = buildNamespaceFromEntityName entityName
        let typeParamStr, typeConstraintClause = buildTypeParameters entityName tps tps // or [] tps ???
        let heritageStr = buildClassHeritage entityName tps heritage
        awaitDeclaration (entityName, name) entityName heritage (fun memberTableDelayed ->
            let memberTable = memberTableDelayed.Value
            let adjustedEls =
                bodyEls |> List.map (fun el ->
                    match el with
                    | AmbientMemberDeclaration decl ->
                        match decl with
                        | AmbientMethodDeclaration(acc, s, propertyName, CallSignature(tps, ps, _)) ->
                            match memberTable.TryFind propertyName with
                            | None -> el
                            | Some ta -> 
                                AmbientMemberDeclaration(
                                    AmbientMethodDeclaration(acc, s, propertyName, 
                                        CallSignature(tps, ps, ta)))
                        | AmbientPropertyDeclaration(acc, s, propertyName, _) ->
                            match memberTable.TryFind propertyName with
                            | None -> el
                            | Some ta -> 
                                AmbientMemberDeclaration(
                                    AmbientPropertyDeclaration(acc, s, propertyName, ta))
                    | _ -> el)
            buildDeclaration 
                nsCs name (typeParamStr, typeConstraintClause)
                heritageStr
                (adjustedEls |> List.map (buildClassBodyElement entityName name tps))
        )

    and buildTypeMember propMap entityName className classTps typeKind = function
        | MemberPropertySignature(PropertySignature(name, _, typeAnnotation)) ->
            buildPropertyDeclaration propMap entityName classTps NonStatic name typeAnnotation |> Some
        | MemberCallSignature callSignature ->
            buildCallDeclaration entityName classTps NonStatic typeKind callSignature |> Some
        | MemberConstructSignature(ConstructSignature(tps, ps, ta)) ->
            //TODO: Should we consider the type params here?
            match typeKind with
            | Class -> buildConstructorDeclaration entityName className classTps tps ps ta |> Some
            | Interface -> 
                buildObjectType 
                    Class entityName (className + "Factory") 
                    classTps (ClassHeritage(None, [])) (ObjectType [MemberConstructSignature(ConstructSignature(tps, ps, ta))])
                None
        | MemberIndexSignature indexSignature ->
            buildIndexSignature entityName classTps typeKind indexSignature |> Some
        | MemberMethodSignature(MethodSignature(name, _, callSignature)) ->
            buildMethodDeclaration propMap entityName classTps NonStatic typeKind name callSignature |> Some

    and buildObjectType typeKind entityName name tps heritage (ObjectType members) =
        let nsCs, nsJs = buildNamespaceFromEntityName entityName
        let typeParamStr, typeConstraintClause = buildTypeParameters entityName tps tps // or [] tps ???
        let heritageStr = buildClassHeritage entityName tps heritage
        let buildDecl =
            match typeKind with
            | Class -> buildDeclaration
            | Interface -> buildInterfaceDeclaration
        awaitDeclaration (entityName, name) entityName heritage (fun memberTableDelayed ->
            let memberTable = memberTableDelayed.Value
            let adjustedMembers =
                members 
                |> List.map (fun m ->
                    match m with
                    | MemberMethodSignature
                            (MethodSignature(propName, isOpt, CallSignature(tps,ps,_))) ->
                        match memberTable.TryFind propName with
                        | Some ta -> 
                            MemberMethodSignature(
                                MethodSignature(propName, isOpt, CallSignature(tps,ps,ta)))
                        | None -> m
                    | MemberPropertySignature
                            (PropertySignature(propName, isOpt, _)) ->
                        match memberTable.TryFind propName with
                        | Some ta -> 
                            MemberPropertySignature(
                                PropertySignature(propName, isOpt, ta))
                        | None -> m
                    | _ -> m)
            let propMap = 
                match typeKind with
                | Interface -> Some(ref Map.empty)
                | Class -> None
            let codeToEmit =
                adjustedMembers
                |> List.choose (buildTypeMember propMap entityName name tps typeKind)
                |> List.map (fun ((_, staticness, _) as x) ->
                    assert(typeKind <> Interface || staticness = NonStatic); x)
            buildDecl 
                nsCs name (typeParamStr, typeConstraintClause)
                heritageStr
                codeToEmit
            propMap |> Option.iter (fun propMap ->
                notifyDeclaration entityName name tps !propMap)
        )

    let buildInterfaceDeclaration entityName 
        (InterfaceDeclaration(name, tps, InterfaceExtendsClause extends, objType)) =
        let heritage = ClassHeritage(None, extends)
        buildObjectType Interface entityName name tps heritage objType

    let buildAmbientEnumMember = function
        | NamedCase propertyName -> 
            buildPropertyName propertyName |> fst
        | NumberedCase(propertyName, i) ->
            let name = buildPropertyName propertyName |> fst
            sprintf "%s = %i" name i

    let buildAmbientEnumDeclaration entityName name members =
        let nsCs, _ = buildNamespaceFromEntityName entityName
        let membersDef =
            members |> List.map buildAmbientEnumMember
            |> String.concat ", "
        match nsCs with
        | None ->
            codef """
public enum %s
{
    %s
}
"""             name membersDef
        | Some nsCs ->
            codef """
namespace %s {
    public enum %s
    {
        %s
    }
}"""            nsCs name membersDef

    let buildCommonAmbientElement entityName = function
        | AmbientVariableDeclaration(name, typeAnnotation) ->
            buildVariableDeclaration entityName [] name typeAnnotation
        | AmbientFunctionDeclaration(name, callSignature) ->
            buildFunctionDeclaration entityName [] name callSignature
        | AmbientClassDeclaration(name, tps, heritage, bodyEls) ->
            buildClassDeclaration entityName name tps heritage bodyEls
        | AmbientEnumDeclaration(name, members) ->
            buildAmbientEnumDeclaration entityName name members

    let (@.) (EntityName firstParts) (EntityName secondParts) =
        EntityName(firstParts @ secondParts)

    let rec buildAmbientModuleElement entityName = function
        | AmbientModuleElement(_, el) -> 
            buildCommonAmbientElement entityName el
        | InterfaceDeclarationElement(_, interfaceDecl) ->
            buildInterfaceDeclaration entityName interfaceDecl
        | AmbientModuleDeclarationElement(_, subEntityName, els) ->
            let fullEntityname = entityName @. subEntityName
            els |> List.iter
                (buildAmbientModuleElement fullEntityname)
        | ImportDeclarationElement _ -> ()

    let buildAmbientExternalModuleElement entityName = function
        | ExternalAmbientModuleElement el ->
            buildAmbientModuleElement entityName el
        | ExternalExportAssignment _
        | ExternalAbientImportDeclaration _ -> ()

    let buildAmbientDeclaration entityName = function
        | CommonAmbientElementDeclaration el -> 
            buildCommonAmbientElement entityName el
        | AmbientModuleDeclaration(subEntityName, els) ->
            let fullEntityname = entityName @. subEntityName
            els |> List.iter
                (buildAmbientModuleElement fullEntityname)
        | AmbientExternalModuleDeclaration(name, els) ->
            let subNames = 
                name.Split [|'.'|] 
                |> Array.map (fun name -> name.Replace(" ", ""))
                |> Array.toList
            let fullEntityName = entityName @. (EntityName subNames)
            els |> List.iter
                (buildAmbientExternalModuleElement fullEntityName)

    let buildModuleNameFromPath modulePath =
        let moduleName = System.IO.Path.GetFileName modulePath
        let moduleName =
            if moduleName.EndsWith ".d.ts" then 
                moduleName.Substring(0, moduleName.Length - ".d.ts".Length)
            else moduleName
        let saneName = sanitise(moduleName).ToLower()
        saneName

    let buildDeclarationElement = function
        | RootExportAssignment _
        | RootImportDeclaration _ 
        | RootExternalImportDeclaration _ -> ()
        | RootReference ref ->
            match ref with
            | File modulePath ->
                let saneName = buildModuleNameFromPath modulePath
                codef "using FunScript.TypeScript.%s;" saneName
            | NoDefaultLib _ -> ()
        | RootInterfaceDeclaration(_, inter) -> 
            buildInterfaceDeclaration (EntityName []) inter
        | RootAmbientDeclaration(_, decl) -> 
            buildAmbientDeclaration (EntityName []) decl

    let definitionsModules =
        definitionFiles |> List.map (fun (name, decls) ->
            let moduleName = (sanitise name).ToLower()
            moduleName, decls)

    let getDependencies (moduleName, DeclarationsFile decls) =
        let isLib =
            decls |> List.exists (function
                | RootReference(NoDefaultLib true) -> true
                | _ -> false)
        let rest =
            decls |> List.choose (function
                | RootReference(File path) -> Some(buildModuleNameFromPath path)
                | _ -> None)
        if isLib then (moduleName, decls), rest
        else (moduleName, decls), "lib" :: rest
                
    let orderedDeclarations =
        let rec sortByDependency modules =
            seq {
                match modules with
                | [] -> ()
                | _ ->
                    let noDeps, someDeps =
                        modules |> List.partition (fun (_, unresolved, _) -> unresolved = [])

                    if noDeps = [] then
                        someDeps |> List.map (fun ((name, _), unresolved, _) -> name, unresolved)
                        |> failwithf  "TypeScript reference cycle detected: \n%A" 

                    yield! noDeps |> Seq.map (fun ((name, decls), _, allDeps) -> name, decls, allDeps)

                    let resolved = noDeps |> Seq.map (fun ((name, _), _, _) -> name) |> set

                    let nextDeps =
                        someDeps |> List.map (fun (nameAndDecls, unresolved, allDeps) ->
                            nameAndDecls, 
                            unresolved |> List.filter (resolved.Contains >> not),
                            allDeps)

                    yield! sortByDependency nextDeps
            }
        let initialDependencies = 
            definitionsModules |> List.map getDependencies
            |> List.map (fun (x, ys) -> x, ys, ys)
        sortByDependency initialDependencies |> Seq.toArray

    let outputFiles =
        orderedDeclarations |> Array.map (fun (moduleName, decls, allDeps) ->
            printfn "Generating types for %s..." moduleName
            //if moduleName <> "lib" then
                //codef "using FunScript.TypeScript.lib;"
            //codef "namespace FunScript.TypeScript.%s {" moduleName
            decls |> List.iter buildDeclarationElement
            //codef "}"
            let output = code.ToString()
            code.Clear() |> ignore
            moduleName, allDeps, output)
    for KeyValue((entityName, count), listeners) in awaitingDeclaration do
        let listenerIds = listeners |> Map.toArray |> Array.map (snd >> fst)
        let typeName = buildTypeNameFromEntityName entityName
        printfn "[WARN] '%s`%i' did not satisfy some dependencies: \n%A" typeName count listenerIds
    outputFiles