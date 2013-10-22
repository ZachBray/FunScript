module internal FunScript.TypeScript.TypeGenerator

open System.Text
open System.Collections.Generic
open Microsoft.FSharp.Core.Printf
open FunScript.TypeScript.AST 

module List =
    let topologicalSortBy selectKey selectDependencies xs =
        let rec topologicalSortBy xs =
            seq {
                match xs with
                | [] -> ()
                | _ ->
                    let noDeps, someDeps =
                        xs |> List.partition (fun (_, unresolved, _) -> unresolved = [])

                    if noDeps = [] then
                        someDeps |> List.map (fun (key, unresolved, _) -> key, unresolved)
                        |> failwithf  "Cycle detected: \n%A" 

                    yield! noDeps |> Seq.map (fun (_, _, out) -> out)

                    let resolved = noDeps |> Seq.map (fun (key, _, _) -> key) |> set

                    let nextDeps =
                        someDeps |> List.map (fun (key, unresolved, out) ->
                            key, 
                            unresolved |> List.filter (resolved.Contains >> not),
                            out)

                    yield! topologicalSortBy nextDeps
            }
        xs 
        |> List.map (fun x -> 
            let key = selectKey x
            let dependencies = selectDependencies x
            key, dependencies, (x, key, dependencies))
        |> topologicalSortBy

[<AutoOpen>]
module private Domain =

    type Staticness =
        | Static
        | NonStatic

    type Name = string

    type TypeRef = 
        | TypeRef of string list * TypeRef list

        static member exists f x =
            let rec existsAux f (TypeRef(n, ts) as t) =
                f t || ts |> List.exists (existsAux f)
            existsAux f x

        static member map f x =
            let rec mapAux f (TypeRef(n, xs)) =
                f (TypeRef(n, xs |> List.map (mapAux f)))
            mapAux f x

        static member collect f x =
            let rec collectAux f (TypeRef(n, xs) as t) =
                seq {
                    yield! xs |> Seq.collect (collectAux f)
                    yield f t
                }
            collectAux f x

        static member mapName f x =
            TypeRef.map (fun (TypeRef(n, ps)) -> TypeRef(f n, ps)) x

    type GenericConstraint = TypeRef * TypeRef
    type SuperType = TypeRef
    type ParentType = TypeRef
    type ReturnType = TypeRef
    type GenericParameter = TypeRef
    type ParameterType = TypeRef

    type FFIMemberParameter =
        | Fixed of string
        | Variable of string * ParameterType
        | InlineArray of string * ParameterType

    type FFIMemberElement =
        | Method of GenericParameter list * GenericConstraint list * FFIMemberParameter list * ReturnType
        | Index of ParameterType * ReturnType
        | Property of ReturnType

    type FFIElement =
        | Enum of TypeRef * (PropertyName * int option) list
        | Member of ParentType * PropertyName option * Staticness * FFIMemberElement
        | Interface of TypeRef * GenericConstraint list * SuperType list

    let (|Staticness|) = function   
        | true -> Static
        | false -> NonStatic

module private FFIMapping =

    let index = ref 0
    let getIndex() = incr index; !index

    let predefinedTypeToTypeRef x =
        let name =
            match x with
            | Any -> "obj"
            | Number -> "float"
            | Boolean -> "bool"
            | String -> "string"
            | Void -> "unit"
        TypeRef([name], [])

    let rec typeReferenceToTypeRef k parentType (TypeReference(EntityName xs, ys)) =
        TypeRef(xs |> List.rev, ys |> List.map (typeToTypeRef k parentType))

    and typeToTypeRef k parentType = function
        | Predefined x -> predefinedTypeToTypeRef x
        | Reference x -> typeReferenceToTypeRef k parentType x
        | Query _ -> predefinedTypeToTypeRef Any (* TODO *)
        | Literal x -> literalTypeToTypeRef k parentType x

    and literalTypeToTypeRef k parentType = function
        | LiteralObjectType x -> objectTypeToTypeRef k parentType x
        | LiteralArrayType x -> TypeRef(["Array"], [x |> typeToTypeRef k parentType])
        | LiteralFunctionType(_, xs, y)
        | LiteralConstructorType(_, xs, y) -> 
            // TODO: constraints from type parameters
            let rs = parameterListToTypeRefs k parentType xs
            let s = typeToTypeRef k parentType y
            TypeRef(["Func"; "System"], [yield! rs; yield s])

    and objectTypeToTypeRef k (TypeRef(tns, _), isInterface) (ObjectType xs as z) =
        let ns = 
            match isInterface, tns with 
            | _, [] -> []
            | false, ns -> ns
            | true, _::ns -> ns
        let name = sprintf "AnonymousType%i" (getIndex())
        let y = TypeRef(name :: ns, [])
        Interface(y, [], []) |> k
        fromObjectType k y z
        y
        
    and typeAnnotationOptionToTypeRef k parentType = function
        | Some(TypeAnnotation x) -> typeToTypeRef k parentType x
        | None -> predefinedTypeToTypeRef Any

    and parameterListToTypeRefs k parentType xs =
        parameterListToFFIMemberParameters k parentType xs
        |> List.map (function
            | Variable(_, x) | InlineArray(_, x) -> x
            | Fixed _ -> predefinedTypeToTypeRef String)

    and parameterListToFFIMemberParameters k parentType (ParameterList(xs, ys, z)) =
        [
            yield! xs |> Seq.map (requiredParameterToMemberParameter k parentType)
            yield! ys |> Seq.map (optionalParameterToMemberParameter k parentType)
            yield! z |> Option.toArray |> Seq.map (restParameterToMemberParameter k parentType)
        ]

    and toLowerCamelCase(str : string) =
        str.[0..0].ToLower() + str.[1..]
        
    and requiredParameterToMemberParameter k parentType = function
        | VariableParameter(_, x, y) -> Variable(toLowerCamelCase x, typeAnnotationOptionToTypeRef k parentType y)
        | StringEnumParameter(_, x) -> Fixed(toLowerCamelCase x)

    and optionalParameterToMemberParameter k parentType = function
        | OptionalParameter(_, x, y)
        | DefaultParameter(_, x, y, _) -> Variable(toLowerCamelCase x, typeAnnotationOptionToTypeRef k parentType y)

    and restParameterToMemberParameter k parentType (RestParameter(n, x)) =
        let y = match x with Some(TypeAnnotation z) -> z | None -> Predefined Any
        let t = literalTypeToTypeRef k parentType (LiteralArrayType y)
        InlineArray(toLowerCamelCase n, t)

    and typeParameterToTypeRef(TypeParameter(name, _)) = TypeRef([name], [])

    and typeParameterToTypeConstraint k parentType (TypeParameter(_, ys) as x) =
        ys |> Option.map (fun y -> typeParameterToTypeRef x, typeToTypeRef k parentType y)

    and interfaceExtendsClauseToTypeRefs k parentType (InterfaceExtendsClause xs) =
        xs |> List.map (typeReferenceToTypeRef k parentType)

    and classHeritageToTypeRefs k parentType (ClassHeritage(x, ys)) =
        let zs = ys |> List.map (typeReferenceToTypeRef k parentType)
        match x with
        | None -> zs
        | Some r -> typeReferenceToTypeRef k parentType r :: zs

    and fromSignature k x y z xs ys r =
        let genericParameters = xs |> List.map typeParameterToTypeRef
        let constraints = xs |> List.choose (typeParameterToTypeConstraint k x)
        let parameters = parameterListToFFIMemberParameters k x ys
        let meth = Method(genericParameters, constraints, parameters, r)
        Member(fst x, y, z, meth) |> k : unit

    and fromCallSignature k x y z (CallSignature(xs, ys, zs)) =
        let returnType = typeAnnotationOptionToTypeRef k x zs
        fromSignature k x y z xs ys returnType

    and fromConstructSignature k x (ConstructSignature(xs, ys, zs)) =
        let returnType =
            match zs with
            | None -> fst x
            | Some _ -> typeAnnotationOptionToTypeRef k x zs
        fromSignature k x None Static xs ys returnType

    and fromIndexSignature k x y =
        let n, z, r =
            match y with
            | IndexSignatureNumber(n, TypeAnnotation z) -> n, z, TypeRef(["int"], [])
            | IndexSignatureString(n, TypeAnnotation z) -> n, z, predefinedTypeToTypeRef String
        let returnType = typeToTypeRef k x z
        Member(fst x, None, NonStatic, Index(r, returnType)) |> k

    and fromMethodSignature k x s (MethodSignature(y, _, z)) =
        fromCallSignature k x (Some y) s z

    and fromPropertySignature k x s (PropertySignature(y, _, z)) =
        let returnType = typeAnnotationOptionToTypeRef k x z
        Member(fst x, Some y, s, Property returnType) |> k

    and fromTypeMember k x = function
        | MemberCallSignature y -> fromCallSignature k x None NonStatic y
        | MemberConstructSignature y -> fromConstructSignature k x y
        | MemberIndexSignature y -> fromIndexSignature k x y
        | MemberMethodSignature y -> fromMethodSignature k x NonStatic y
        | MemberPropertySignature y -> fromPropertySignature k x NonStatic y

    and fromObjectType k x (ObjectType ys) = 
        for y in ys do fromTypeMember k (x, true) y

    and fromInterface k ns (InterfaceDeclaration(name, xs, ys, z)) =
        let parameters = xs |> List.map typeParameterToTypeRef
        let x = TypeRef(name::ns, parameters)
        let constraints = xs |> List.choose (typeParameterToTypeConstraint k (x, true))
        let superTypes = interfaceExtendsClauseToTypeRefs k (x, true) ys
        Interface(x, constraints, superTypes) |> k
        fromObjectType k x z

    and fromAmbientClassBodyElement k x = function
        | AmbientConstructorDeclaration y -> 
            fromConstructSignature k x (ConstructSignature([], y, None))
        | AmbientIndexSignature y ->
            fromIndexSignature k x y
        | AmbientMemberDeclaration(AmbientMethodDeclaration(o, Staticness y, z, r)) ->
            fromMethodSignature k x y (MethodSignature(z, false, r))
        | AmbientMemberDeclaration(AmbientPropertyDeclaration(o, Staticness y, z, r)) ->
            fromPropertySignature k x y (PropertySignature(z, false, r))

    and fromClassDeclaration k ns n xs y zs =
        let parameters = xs |> List.map typeParameterToTypeRef
        let x = TypeRef(n::ns, parameters)
        let constraints = xs |> List.choose (typeParameterToTypeConstraint k (x, true))
        let superTypes = classHeritageToTypeRefs k (x, true) y
        Interface(x, constraints, superTypes) |> k
        for z in zs do fromAmbientClassBodyElement k (x, true) z

    and fromEnumDeclaration k ns n xs =
        let x = TypeRef(n::ns, [])
        let members = 
            xs |> List.map (function
                | NamedCase n -> n, None
                | NumberedCase(n, i) -> n, Some i)
        Enum(x, members) |> k

    and fromFunctionDeclaration k ns n x =
        let t = TypeRef(ns, [])
        let y = Some(NameIdentifier n)
        fromCallSignature k (t, false) y Static x

    and fromVariableDeclaration k ns n x =
        let t = TypeRef(ns, [])
        let y = NameIdentifier n
        fromPropertySignature k (t, false) Static (PropertySignature(y, false, x))

    and fromCommonAmbientElement k ns = function
        | AmbientClassDeclaration(n, xs, y, zs) -> fromClassDeclaration k ns n xs y zs
        | AmbientEnumDeclaration(n, xs) -> fromEnumDeclaration k ns n xs
        | AmbientFunctionDeclaration(n, x) -> fromFunctionDeclaration k ns n x
        | AmbientVariableDeclaration(n, x) -> fromVariableDeclaration k ns n x

    and fromAmbientModuleElement k ns = function
        | AmbientModuleDeclarationElement(_, EntityName xs, ys) ->
            ys |> List.iter (fromAmbientModuleElement k ((xs |> List.rev) @ ns))
        | AmbientModuleElement(_, x) ->
            fromCommonAmbientElement k ns x
        | ImportDeclarationElement _ -> ()
        | InterfaceDeclarationElement(_, x) ->
            fromInterface k ns x

    and fromAmbientExternalModuleElement k ns = function  
        | ExternalAbientImportDeclaration _ 
        | ExternalExportAssignment _ -> ()
        | ExternalAmbientModuleElement x -> fromAmbientModuleElement k ns x 

    and fromAmbientDeclaration k ns = function
        | CommonAmbientElementDeclaration x -> 
            fromCommonAmbientElement k ns x
        | AmbientExternalModuleDeclaration(n, xs) -> 
            xs |> List.iter (fromAmbientExternalModuleElement k (n::ns))
        | AmbientModuleDeclaration(EntityName xs, ys) ->
            ys |> List.iter (fromAmbientModuleElement k ((xs |> List.rev) @ ns))

    and fromDeclarationElement k = function
        | RootExportAssignment _
        | RootImportDeclaration _ 
        | RootExternalImportDeclaration _
        | RootReference _ -> ()
        | RootInterfaceDeclaration(_, x) -> fromInterface k [] x
        | RootAmbientDeclaration(_, x) -> fromAmbientDeclaration k [] x

module Identifier =

    let csharpKeywords =
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

    let fsharpKeywords =
        Set [|"async"; "method"; "atomic"; "mixin"; "break"; "namespace"; "checked";
            "object"; "component"; "process"; "const"; "property"; "constraint";
            "protected"; "constructor"; "public"; "continue"; "pure"; "decimal";
            "readonly"; "eager"; "return"; "enum"; "sealed"; "event"; "switch";
            "external"; "virtual"; "fixed"; "void"; "functor"; "volatile"; "include";
            "where"; "abstract"; "lsl"; "and"; "lsr"; "as"; "lxor"; "assert"; "match";
            "member"; "asr"; "mod"; "begin"; "module"; "class"; "mutable"; "namespace";
            "default"; "new"; "delegate"; "null"; "do"; "of"; "done"; "open";
            "downcast"; "or"; "downto"; "override"; "else"; "rec"; "end"; "sig";
            "exception"; "static"; "false"; "struct"; "finally"; "then"; "for"; "to";
            "fun"; "true"; "function"; "try"; "if"; "type"; "in"; "val"; "inherit";
            "when"; "inline"; "upcast"; "interface"; "while"; "land"; "with"; "lor";
            "measure"; "atomic"; "break"; "checked"; "component"; "const";
            "constraint"; "constructor"; "continue"; 
            "eager"; "event"; "external"; "fixed"; "functor"; "include";
            "method"; "mixin"; "object"; "parallel"; "process"; "protected"; "pure";
            "sealed"; "tailcall"; "trait"; "virtual"; "volatile"; "asr"; "land"; "lor";
            "lsl"; "lsr"; "lxor"; "mod"; "sig"; "int"; "sbyte"; "byte"; "uint";
            "uint32"; "int32"; "int64"; "uint64"; "int16"; "uint16"; "float";
            "decimal"; "float32"; "bool"; "obj"; "unit"; "global" |]

    let keywords = csharpKeywords + fsharpKeywords

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
                        | '\'' -> "'"
                        | '_' -> "_"
                        | c when c >= 'a' && c <= 'z' -> c.ToString()
                        | c when c >= 'A' && c <= 'Z' -> c.ToString()
                        | c when c >= '0' && c <= '9' -> c.ToString()
                        | c -> sprintf "CHAR%i" (int c))
                let firstC = almostSane.[0] 
                if firstC >= '0' && firstC <= '9' then "_" + almostSane
                else almostSane

    let fromPropertyName = function
        | NameIdentifier n -> sanitise n, sprintf ".%s" n
        | NameNumericLiteral i -> sprintf "``%i``" i, sprintf "[%i]" i
        | NameStringLiteral s -> sprintf "``%s``" s, sprintf "[\"%s\"]" s

module private Generate =
    
    /// Creates a key based on what F# would consider a unique type.
    /// For example IFoo<'a, 'b> and IFoo<'b, 'c> would clash and
    /// must be merged for this reason.
    let typeRefToTypeKey(TypeRef(ns, ps)) = 
        ns, ps.Length

    /// Groups ffi elements by the name of the parent type and 
    /// its number of generic parameters.
    let groupByParentType xs =
        xs |> Seq.groupBy (function
            | Enum(x, _)
            | Interface(x, _, _)
            | Member(x, _, _, _) -> typeRefToTypeKey x)
        |> Seq.map (fun (x, ys) -> x, set ys)
        |> Map.ofSeq

    let predefinedTypeRefs =
        TypeRef(["int"], []) ::
        List.map (FFIMapping.predefinedTypeToTypeRef)  [
            Any
            Number
            Boolean
            String
            Void
        ]

    let predefinedNs =
        set (predefinedTypeRefs |> List.map (fun (TypeRef(ns, _)) -> ns))

    let predefinedTypeKeys =
        set (predefinedTypeRefs |> List.map typeRefToTypeKey)

    let nameCode ns =
        match ns with
        | [] -> None
        | ["Array"] -> Some "array"
        | _ -> 
            let sanitise =
                if predefinedNs.Contains ns then id
                else Identifier.sanitise
            ns |> List.rev |> List.map sanitise |> String.concat "." |> Some

    let namespaceCode ns =
        match nameCode ns with
        | None -> "global"
        | Some namespaceCode -> namespaceCode

    let extractNamespaceCode = function
        | TypeRef([], _) -> failwith "Expected a valid TypeRef"
        | TypeRef(_::ns, _) -> namespaceCode ns

    /// Tries to return a (name * namespace * parameter list) tuple for declaring
    /// types. This doesn't search the type space to resolve the type.
    let (|TypeDeclarationId|_|) = function
        | TypeRef(n::ns, gps) -> Some(Identifier.sanitise n, namespaceCode ns, gps)
        | _ -> None

    let enumCode t ms =
        match t with
        | TypeDeclarationId(nameId, namespaceId, []) ->
            let cases = 
                ms |> List.mapi (fun i (x, y) -> 
                    Identifier.fromPropertyName x |> fst, defaultArg y i)
            let code =
                [
                    yield sprintf """
namespace %s

type %s = """               namespaceId nameId
                
                    for name, i in cases do
                        yield sprintf """
        | %s = %i"""            name i
                ] |> String.concat ""
            Some(code, Set.empty, typeRefToTypeKey t, None)
        | _ -> 
            printfn "[ERROR] Invalid Enum TypeRef: %A" t
            None
        
    /// Tries to resolve a TypeRef from the generic type parameters at the location
    /// or the known types in the current module or the referenced modules.
    let resolveTypeRef typesByKey (TypeRef(baseNs,_) as baseT) genericTypeParameters t =

        let rec tryResolve basePart ((finalPart, paramCount) as x) = 
            let typeSpace = finalPart @ basePart
            let hasType = 
                typeSpace = ["Func"; "System"] ||
                (typeSpace = ["Array"] && paramCount <= 1) ||
                predefinedTypeKeys.Contains (typeSpace, paramCount) ||
                typesByKey |> List.exists (fun f -> f (typeSpace, paramCount))
            if hasType then Some typeSpace
            else
                match basePart with
                | [] -> None
                | _::subBasePart -> tryResolve subBasePart x

        t |> TypeRef.map (fun (TypeRef(_, ps) as t) ->
            if genericTypeParameters |> Set.contains t then t
            else
                match tryResolve baseNs (typeRefToTypeKey t) with
                | Some r -> TypeRef(r, ps)
                | None -> failwithf "[ERROR] Unable to resolve TypeRef: %A (from TypeRef: %A)" t baseT)

    /// Returns the F# string representing the type ref. It assumes that the generic
    /// parameters have been mapped into F# friendly back-ticked type refs and that
    /// all types have been resolved.
    let rec typeReferenceCode (TypeRef(ns, gps)) =
        let nameCode = nameCode ns
        match nameCode, gps with
        | None, _ -> failwith "[ERROR] Cannot emit empty TypeRef reference."
        | Some "array", [] -> "array<obj>"
        | Some nc, [] -> nc
        | Some nc, gps -> 
                let parameters = gps |> Seq.map typeReferenceCode |> String.concat ", "
                sprintf "%s<%s>" nc parameters

    /// This is to work around the restriction where you cannot have a type parameter
    /// that `a :> `b[]
    let replaceArrayWithIList x =
        x |> TypeRef.map (function
            | TypeRef(["Array"], ps) -> TypeRef(List.rev ["System"; "Collections"; "Generic"; "IList"], ps)
            | x -> x)

    /// Builds the code segment specifying a sub type relationship type constraint.
    let constraintCode gps (x,y) =
        match x with
        | TypeRef([gn], []) ->
            Some(sprintf "%s :> %s" (typeReferenceCode x) (typeReferenceCode (replaceArrayWithIList y)))
        | _ -> 
            printfn "[WARN] Ignoring incompatible constraint: %A :> %A" x y
            None

    let constrainedGenericParameterCode gps gcs =
        match gps with
        | [] -> None
        | _ -> 
            let parameters = gps |> Seq.map typeReferenceCode |> String.concat ", "
            match gcs with
            | [] -> Some(sprintf "<%s>" parameters)
            | _ ->
                let constraintProperties = 
                    gcs |> List.choose (constraintCode gps) |> String.concat " and "
                Some(sprintf "<%s when %s>" parameters constraintProperties)

    let constrainedTypeReferenceCode (TypeRef(ns, gps)) gcs  =
        let nameCode = nameCode ns
        match nameCode with
        | None -> failwith "[ERROR] Cannot emit empty TypeRef reference."
        | Some nc ->
            match constrainedGenericParameterCode gps gcs with
            | None -> nc, ns |> List.head
            | Some genericDef -> nc + genericDef, (ns |> List.head) + genericDef

    let fixGenericParameterName = function
        | TypeRef([gn], []) -> TypeRef([sprintf "'%s" gn], [])
        | x -> failwithf "[ERROR] Invalid Generic Parameter TypeRef: %A" x


    /// Replaces all references to generic parameters in the given type ref
    /// with an F# friendly generic parameter name.
    let replaceGenericParameters gps x =
        x |> TypeRef.map (fun t -> 
            defaultArg (gps |> Map.tryFind t) t)

    let fixGenericParameters gps =
        let fixedGps = gps |> List.map fixGenericParameterName
        let fixedGpsSet = set fixedGps
        let gpsFixMapping = List.zip gps fixedGps |> Map.ofList
        let fix = replaceGenericParameters gpsFixMapping
        fixedGps, fixedGpsSet, fix

    let interfaceCode typesByKey (TypeRef(ns, gps) as t) gcs sts =
        try
            let fixedGps, fixedGpsSet, fix = fixGenericParameters gps
            let fix = fix >> resolveTypeRef typesByKey t fixedGpsSet
            let fixedGcs = gcs |> List.map (fun (x, y) -> fix x, fix y)
            let fixedSts = sts |> List.map fix
            let dependencies = 
                let gpsKeys = fixedGpsSet |> Set.map typeRefToTypeKey
                let possibleDeps = fixedSts @ (fixedGcs |> List.map snd)
                let expandedDeps = possibleDeps |> Seq.collect (TypeRef.collect typeRefToTypeKey) |> set
                expandedDeps - gpsKeys
            let interfaceSignature, localSignature =
                constrainedTypeReferenceCode (TypeRef(ns, fixedGps)) fixedGcs
            //TypeRef(ns, fixedGps), fixedGcs, fixedSts, interfaceSignature

            let interfaceDeclaration =
                sprintf """
namespace %s
type %s ="""        (extractNamespaceCode t) localSignature
            let interfaceBodyEls =
                fixedSts |> List.map typeReferenceCode
                |> List.map (fun typeRef ->
                    sprintf """
        inherit %s""" typeRef)
            let interfaceBody =
                match interfaceBodyEls with
                | [] -> " interface end"
                | _ -> interfaceBodyEls |> String.concat ""
            Some(interfaceDeclaration + interfaceBody + System.Environment.NewLine,
                 dependencies,
                 typeRefToTypeKey t, 
                 Some(interfaceSignature, fixedGps))
        with ex ->
            printfn "%s" ex.Message
            None
        

    /// Returns the code of the type declarations needed for a given
    /// module of FFI elements. This filters out the types already
    /// declared in dependencies. Also, returns a map from type key
    /// to type signature.
    let typeDeclarationsAndSignatures (moduleElements : Map<_,_>) typesByKey =
        let typesByKeyInclCurrentModule =
            moduleElements.ContainsKey :: typesByKey

        moduleElements |> Map.toSeq |> Seq.choose (fun (typeKey, els) ->
            let wasAlreadyDeclared = typesByKey |> List.exists (fun f -> f typeKey)
            if wasAlreadyDeclared then None
            else 
                let typeFound =
                    els |> Seq.tryPick (function    
                        | Interface(t, gcs, sts) -> interfaceCode typesByKeyInclCurrentModule t gcs sts
                        | Enum(t, ms) -> enumCode t ms
                        | Member _ -> None)
                match typeFound with
                | None -> interfaceCode typesByKeyInclCurrentModule (TypeRef("Globals" :: fst typeKey, [])) [] []
                | _ -> typeFound
        ) |> Seq.toList
        |> List.topologicalSortBy 
            (fun (_, _, x, _) -> x) 
            (fun (_, xs, _, _) -> 
                xs |> Set.toList |> List.filter (fun x -> 
                    typesByKey |> List.exists (fun f -> f x) |> not &&
                    predefinedTypeKeys |> Set.contains x |> not))
        |> Seq.map (fun (x, _, _) -> x)
        |> Seq.fold (fun (allCode : StringBuilder, typeSigs) (codeFragment, _, typeKey, typeSignature) ->
            allCode.Append codeFragment, 
            typeSigs |> Map.add typeKey typeSignature
        ) (StringBuilder(), Map.empty)
        |> fun (allCode, typeSigs) -> allCode.ToString(), typeSigs
        

    /// Creates a function to rename a methods generic parameters
    /// where they conflict with the types generic parameters.
    let collisionFix fixedGpsSet methodGps =
        let collisonAvoidanceMap =
            let rec makeSafe count = function
                | TypeRef([n], ps) as t ->
                    let testT =
                        if count = 0 then t
                        else TypeRef([sprintf "%s%i" n count], ps)
                    if fixedGpsSet |> Set.contains testT then makeSafe (count + 1) t
                    else testT
                | t -> t
            methodGps |> List.map (fun t -> t, makeSafe 0 t)
            |> List.filter (fun (x, y) -> x <> y)
            |> Map.ofList
        TypeRef.map (fun x -> defaultArg (collisonAvoidanceMap.TryFind x) x)
        
    let mapParameterType fix = function
        | Variable(name, t) -> Variable(name, fix t)
        | InlineArray(name, t) -> InlineArray(name, fix t)
        | Fixed _ as x -> x

    let parameterCode = function
        | Variable(name, t) -> "", Some(sprintf "%s : %s" (Identifier.sanitise name) (typeReferenceCode t))
        | InlineArray(name, t) -> "", Some(sprintf "[<System.ParamArray>] %s : %s[]" (Identifier.sanitise name) (typeReferenceCode t))
        | Fixed v -> v, None

    let javaScriptTypeCode (TypeRef(ns, _)) =
        ns |> List.rev |> String.concat "."

    let typeExtensionCode typesByKey ns signature fixedGps members =
        let fixedGpsSet = set fixedGps
        let membersCode =
            members |> Seq.map (fun (TypeRef(_, gps) as t, pn, s, el) ->
                let fixMap = List.zip gps fixedGps |> Map.ofList
                let fixGenericTypeParameters = 
                    TypeRef.map (fun x -> defaultArg (fixMap.TryFind x) x)
                let fixedT = fixGenericTypeParameters t
                let name, accessCode = 
                    match pn with
                    | None -> "Create", ""
                    | Some pn -> Identifier.fromPropertyName pn
                let callCode =
                    let baseAccess, seedI =
                        match s with
                        | Static -> javaScriptTypeCode t, 0
                        | NonStatic -> "{0}", 1
                    match el with
                    | Property _ -> baseAccess + accessCode
                    | Method(_, _, ps, _) -> 
                        let parameterCode =
                            ps |> List.fold (fun (i, acc) -> function
                                | Fixed v -> i, sprintf "\"%s\"" v :: acc
                                | Variable _ -> i + 1, sprintf "{%i}" i :: acc
                                | InlineArray _ -> i + 1, sprintf "{%i...}" i :: acc) (seedI, [])
                            |> snd |> List.rev |> String.concat ", "
                        sprintf "%s%s(%s)" baseAccess accessCode parameterCode
                    | Index _ ->
                        sprintf "%s[{%i}]" baseAccess seedI
                let signature =
                    match el with
                    | Property r -> 
                        let rCode = typeReferenceCode(fixGenericTypeParameters r)
                        sprintf "%s with get() : %s = failwith \"never\" and set (v : %s) : unit = failwith \"never\"" name rCode rCode
                    | Method(methodGps, methodGcs, ps, r) ->
                        let partiallyFixedMethodGps, _, fixGenericNames = fixGenericParameters methodGps
                        let collisionFix = fixGenericNames >> collisionFix fixedGpsSet partiallyFixedMethodGps
                        let fixedMethodGps = methodGps |> List.map collisionFix
                        let allGps = set fixedMethodGps + fixedGpsSet
                        let combinedFix = collisionFix >> fixGenericTypeParameters >> resolveTypeRef typesByKey t allGps
                        let fixedGcs = methodGcs |> List.map (fun (x, y) -> combinedFix x, combinedFix y)
                        let fixedPs = ps |> List.map (mapParameterType combinedFix)
                        let specialization, parametersCode = 
                            let specs, pCodes = fixedPs |> List.map parameterCode |> List.unzip
                            specs |> String.concat "", pCodes |> List.choose id |> String.concat ", "
                        let fixedR = combinedFix r
                        let genericDef = defaultArg (constrainedGenericParameterCode fixedMethodGps fixedGcs) ""
                        sprintf "%s%s%s(%s) : %s = failwith \"never\"" 
                            name specialization genericDef parametersCode (typeReferenceCode fixedR)
                    | Index(p, r) ->
                        let pCode = typeReferenceCode(fixGenericTypeParameters p)
                        let rCode = typeReferenceCode(fixGenericTypeParameters r)
                        sprintf "%s with get(i : %s) : %s = failwith \"never\" and set (i : %s) (v : %s) : unit = failwith \"never\"" name pCode rCode pCode rCode
                let root =
                    match s with
                    | Static -> "static member "
                    | NonStatic -> "member __."
                sprintf """
        //[<JSEmitInline(%s)>]
        %s%s"""         callCode root signature 
            ) |> String.concat ""

        let namespaceCode =
            match ns with
            | [] -> None
            | _::ns -> nameCode ns

        let typeExtensionDefinitionCode =
            match signature, namespaceCode with
            | None, None -> 
                sprintf "    type Globals with "
            | None, Some namespaceCode ->
                sprintf "    type %s.Globals with " namespaceCode
            | Some(signature : string), _ -> 
                if signature.StartsWith "array<" then
                    let parts = 
                        signature.Split([|"array<"; ">"|], System.StringSplitOptions.RemoveEmptyEntries)
                    sprintf "    type %s ``[]`` with " parts.[0]
                else
                    sprintf "    type %s with " signature

        System.Environment.NewLine +
        typeExtensionDefinitionCode + System.Environment.NewLine +
        membersCode + System.Environment.NewLine


    let typeExtensions signaturesByKey moduleElements =
        let precursor = """
namespace global

[<AutoOpen>]
module TypeExtensions =

"""
        let signaturesByKeyTest =
            signaturesByKey |> List.map (fun (x : Map<_,_>) -> x.ContainsKey)
        moduleElements |> Map.toSeq |> Seq.choose (fun ((ns, _) as typeKey, els) ->
            let members = 
                els |> Seq.choose (function
                    | Member(t, pn, s, el) -> Some(t, pn, s, el)
                    | _ -> None)
                |> Seq.toList
            match members with
            | [] -> None
            | _ ->
                let out =
                    match signaturesByKey |> List.tryPick (Map.tryFind typeKey) |> Option.bind id with
                    | None -> typeExtensionCode signaturesByKeyTest ns None [] members
                    | Some(signature, fixedGps) -> typeExtensionCode signaturesByKeyTest ns (Some signature) fixedGps members
                Some out)
        |> Seq.fold (fun (acc : StringBuilder) code -> acc.Append code) (StringBuilder().Append precursor)
        |> fun sb -> sb.ToString()

module Compiler =

    let convertPathToModuleName modulePath =
        let moduleName = System.IO.Path.GetFileName modulePath
        let moduleName =
            if moduleName.EndsWith ".d.ts" then 
                moduleName.Substring(0, moduleName.Length - ".d.ts".Length)
            else moduleName
        let saneName = Identifier.sanitise(moduleName).ToLower()
        saneName

    let findModuleDependencies (moduleName, DeclarationsFile decls) =
        let isLib =
            decls |> List.exists (function
                | RootReference(NoDefaultLib true) -> true
                | _ -> false)
        let rest =
            decls |> List.choose (function
                | RootReference(File path) -> Some(convertPathToModuleName path)
                | _ -> None)
        if isLib then rest
        else "lib" :: rest
                
    let orderByCompilationDependencies declarationModules =
        declarationModules |> List.topologicalSortBy 
            fst findModuleDependencies

    let generateTypes declarationFiles =

        let modules =
            declarationFiles |> List.map (fun (name, decls) ->
                let moduleName = (Identifier.sanitise name).ToLower()
                moduleName, decls)

        let modulesInCompilationOrder =
            orderByCompilationDependencies modules
            |> Seq.toList

        let codeByModule =
            modulesInCompilationOrder
            |> List.fold (fun (codeOut, moduleTypes : Map<string, Map<string list * int, (string * TypeRef list) option>>) ((_, DeclarationsFile decls), moduleName, moduleDependencies) ->
                let xs = ResizeArray()
                decls |> List.iter (FFIMapping.fromDeclarationElement xs.Add)
                let elementsByNameKey = 
                    xs |> Seq.groupBy (function
                        | Interface(t, _, _) 
                        | Enum(t, _)
                        | Member(t, _, _, _) -> Generate.typeRefToTypeKey t)
                    |> Seq.map (fun (k, vs) -> k, vs |> Seq.toList)
                    |> Map.ofSeq
                let dependencySigs =
                    moduleDependencies |> List.map (fun n -> moduleTypes |> Map.find n)
                let typesByKey = dependencySigs |> List.map (fun x -> x.ContainsKey)
                let declarationCode, sigs = 
                    Generate.typeDeclarationsAndSignatures elementsByNameKey typesByKey
                let extensionCode =
                    Generate.typeExtensions (sigs :: dependencySigs) elementsByNameKey
                let code = declarationCode + System.Environment.NewLine + extensionCode
                codeOut |> Map.add moduleName code, moduleTypes |> Map.add moduleName sigs
            ) (Map.empty, Map.empty)
            |> fst

        modulesInCompilationOrder |> List.map (fun (_, name, deps) ->
            name, codeByModule |> Map.find name, deps)