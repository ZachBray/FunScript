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
    type Destination = string list
    type Source = string list
    type IsExport = bool

    type Optionality = Required | Optional

    type FFIMemberParameter =
        | Fixed of string
        | Variable of string * ParameterType * Optionality
        | InlineArray of string * ParameterType

    type FFIMemberElement =
        | Method of GenericParameter list * GenericConstraint list * FFIMemberParameter list * ReturnType
        | Index of ParameterType * ReturnType
        | Property of ReturnType
        | FunWrapper

    type FFIPropertyName =
        | Invoker
        | Indexer
        | Constructor
        | Name of PropertyName

    type FFIElement =
        | Enum of TypeRef * (PropertyName * int option) list
        | Member of ParentType * FFIPropertyName * Staticness * FFIMemberElement
        | Interface of TypeRef * GenericConstraint list * SuperType list
        | Delegate of TypeRef * ParameterType list * ReturnType
        | Import of IsExport * Destination * Source

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
            let rs : _ list = parameterListToTypeRefs k parentType xs
            let s = typeToTypeRef k parentType y
            let isRest = match xs with ParameterList(_, _, Some _) -> true | _ -> false
            if isRest then
                functionTypeWithRestToTypeRef k parentType rs s
            else if rs.Length < 9 then
                TypeRef(["Func"; "System"], [yield! rs; yield s])
            else predefinedTypeToTypeRef Any

    and getNs isInterface tns = 
        match isInterface, tns with 
            | _, [] -> []
            | false, ns -> ns
            | true, _::ns -> ns

    and functionTypeWithRestToTypeRef k (TypeRef(tns, classGps), (isInterface, methodGps)) rs s =
        let ns = getNs isInterface tns
        let name = sprintf "RestFuncType%i" (getIndex())
        let paramCount = List.length rs
        let tparams = Seq.init (paramCount + 1) (fun i -> "T" + string i) 
                        |> Seq.map (fun x -> TypeRef([x], [])) 
                        |> List.ofSeq
        let y = TypeRef(name :: ns, tparams)
        Interface(y, [], []) |> k
        let nonRest = 
            tparams 
            |> Seq.take (paramCount - 1) 
            |> Seq.mapi (fun i t -> Variable("x" + string i, t, Required)) 
            |> List.ofSeq
        
        let meth = Method([], [], nonRest @ [ InlineArray("rest", TypeRef(["Array"], [ List.nth tparams (paramCount - 1) ])) ], List.nth tparams paramCount )
        Member(y, Invoker, NonStatic, meth) |> k
        Member(y, Constructor, Static, FunWrapper) |> k

        let restTy = match Seq.last rs with 
                        | TypeRef(["Array"], [t]) -> t
                        | t -> failwith "Expected array type"
        
        TypeRef(name :: ns, ((Seq.take (paramCount - 1) rs) |> List.ofSeq) @ [restTy; s] )

    and objectTypeToTypeRef k (TypeRef(tns, classGps), (isInterface, methodGps)) (ObjectType xs as z) =
        let ns = getNs isInterface tns
        let name = sprintf "AnonymousType%i" (getIndex())
        let y = TypeRef(name :: ns, classGps @ methodGps)
        Interface(y, [], []) |> k
        fromObjectType k y z
        y
        
    and typeAnnotationOptionToTypeRef k parentType = function
        | Some(TypeAnnotation x) -> typeToTypeRef k parentType x
        | None -> predefinedTypeToTypeRef Any

    and parameterListToTypeRefs k parentType xs =
        parameterListToFFIMemberParameters k parentType xs
        |> List.map (function
            | Variable(_, x, _) | InlineArray(_, x) -> x
            | Fixed _ -> predefinedTypeToTypeRef String)

    and parameterListToFFIMemberParameters k parentType (ParameterList(xs, ys, z)) =
        [
            yield! xs |> Seq.map (requiredParameterToMemberParameter k parentType)
            yield! ys |> Seq.map (optionalParameterToMemberParameter k parentType)
            yield! z |> Option.toArray |> Seq.map (restParameterToMemberParameter k parentType)
        ]        

    and requiredParameterToMemberParameter k parentType = function
        | VariableParameter(_, x, y) -> Variable(x, typeAnnotationOptionToTypeRef k parentType y, Required)
        | StringEnumParameter(_, x) -> Fixed x

    and optionalParameterToMemberParameter k parentType = function
        | OptionalParameter(_, x, y)
        | DefaultParameter(_, x, y, _) -> Variable(x, typeAnnotationOptionToTypeRef k parentType y, Optional)

    and restParameterToMemberParameter k parentType (RestParameter(n, x)) =
        let y = match x with Some(TypeAnnotation z) -> z | None -> Literal(LiteralArrayType(Predefined Any))
        // NOTE: we are assuming that the type is in array form already
        //      let t = literalTypeToTypeRef k parentType (LiteralArrayType y)
        InlineArray(n, typeToTypeRef k parentType y)

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

    and parameterListToFFIMemberParametersPermutations k parentType (ParameterList(xs, ys, z)) =
        [
            yield parameterListToFFIMemberParameters k parentType (ParameterList(xs, ys, None))
            match z with
            | None -> ()
            | Some _ ->
                yield 
                    parameterListToFFIMemberParameters k parentType (ParameterList(xs, ys, z))
                    |> List.map (function
                        | Variable(name, t, _) -> Variable(name, t, Required)
                        | x -> x)
        ]

    and fromSignature k (x1, (x2, x3)) y z xs ys r =
        let genericParameters = xs |> List.map typeParameterToTypeRef
        let x = x1, (x2, x3 @ genericParameters)
        let constraints = xs |> List.choose (typeParameterToTypeConstraint k x)
        let paramPermutations = parameterListToFFIMemberParametersPermutations k x ys
        for parameters in paramPermutations do
            let meth = Method(genericParameters, constraints, parameters, r)
            Member(fst x, y, z, meth) |> k : unit

    and fromCallSignature k x y z (CallSignature(xs, ys, zs)) =
        let returnType = typeAnnotationOptionToTypeRef k x zs
        fromSignature k x y z xs ys returnType

    and fromConstructSignature k x y (ConstructSignature(xs, ys, zs)) =
        let returnType =
            match zs with
            | None -> fst x
            | Some _ -> typeAnnotationOptionToTypeRef k x zs
        fromSignature k x Constructor y xs ys returnType

    and fromIndexSignature k x y =
        let n, z, r =
            match y with
            | IndexSignatureNumber(n, TypeAnnotation z) -> n, z, TypeRef(["int"], [])
            | IndexSignatureString(n, TypeAnnotation z) -> n, z, predefinedTypeToTypeRef String
        let returnType = typeToTypeRef k x z
        Member(fst x, Indexer, NonStatic, Index(r, returnType)) |> k

    and fromMethodSignature k x s (MethodSignature(y, _, z)) =
        fromCallSignature k x (Name y) s z

    and fromPropertySignature k x s (PropertySignature(y, _, z)) =
        let returnType = typeAnnotationOptionToTypeRef k x z
        Member(fst x, Name y, s, Property returnType) |> k

    and fromTypeMember k x = function
        | MemberCallSignature y -> fromCallSignature k x Invoker NonStatic y
        | MemberConstructSignature y -> fromConstructSignature k x NonStatic y
        | MemberIndexSignature y -> fromIndexSignature k x y
        | MemberMethodSignature y -> fromMethodSignature k x NonStatic y
        | MemberPropertySignature y -> fromPropertySignature k x NonStatic y

    and fromObjectType k x (ObjectType ys) = 
        for y in ys do fromTypeMember k (x, (true, [])) y

    and fromInterface k ns (InterfaceDeclaration(name, xs, ys, z)) =
        let parameters = xs |> List.map typeParameterToTypeRef
        let x = TypeRef(name::ns, parameters)
        let constraints = xs |> List.choose (typeParameterToTypeConstraint k (x, (true, [])))
        let superTypes = interfaceExtendsClauseToTypeRefs k (x, (true, [])) ys
        Interface(x, constraints, superTypes) |> k
        fromObjectType k x z

    and fromAmbientClassBodyElement k x = function
        | AmbientConstructorDeclaration y -> 
            fromConstructSignature k x Static (ConstructSignature([], y, None))
        | AmbientIndexSignature y ->
            fromIndexSignature k x y
        | AmbientMemberDeclaration(AmbientMethodDeclaration(o, Staticness y, z, r)) ->
            fromMethodSignature k x y (MethodSignature(z, false, r))
        | AmbientMemberDeclaration(AmbientPropertyDeclaration(o, Staticness y, z, r)) ->
            fromPropertySignature k x y (PropertySignature(z, false, r))

    and fromClassDeclaration k ns n xs y zs =
        let parameters = xs |> List.map typeParameterToTypeRef
        let x = TypeRef(n::ns, parameters)
        let constraints = xs |> List.choose (typeParameterToTypeConstraint k (x, (true, [])))
        let superTypes = classHeritageToTypeRefs k (x, (true, [])) y
        Interface(x, constraints, superTypes) |> k
        for z in zs do fromAmbientClassBodyElement k (x, (true, [])) z

    and fromEnumDeclaration k ns n xs =
        let x = TypeRef(n::ns, [])
        let members = 
            xs |> List.map (function
                | NamedCase n -> n, None
                | NumberedCase(n, i) -> n, Some i)
        Enum(x, members) |> k

    and fromFunctionDeclaration k ns n x =
        let t = TypeRef(ns, [])
        let y = Name(NameIdentifier n)
        fromCallSignature k (t, (false, [])) y Static x

    and fromVariableDeclaration k ns n x =
        let t = TypeRef(ns, [])
        let y = NameIdentifier n
        fromPropertySignature k (t, (false, [])) Static (PropertySignature(y, false, x))

    and fromCommonAmbientElement k ns = function
        | AmbientClassDeclaration(n, xs, y, zs) -> fromClassDeclaration k ns n xs y zs
        | AmbientEnumDeclaration(n, xs) -> fromEnumDeclaration k ns n xs
        | AmbientFunctionDeclaration(n, x) -> fromFunctionDeclaration k ns n x
        | AmbientVariableDeclaration(n, x) -> fromVariableDeclaration k ns n x

    and fromImportDeclaration k ns (ImportDeclaration(x, EntityName xs)) y =
        k (Import(y, x::ns, xs |> List.rev))

    and fromAmbientModuleElement k ns = function
        | AmbientModuleDeclarationElement(_, EntityName xs, ys) ->
            ys |> List.iter (fromAmbientModuleElement k ((xs |> List.rev) @ ns))
        | AmbientModuleElement(_, x) ->
            fromCommonAmbientElement k ns x
        | ImportDeclarationElement(y, x) -> 
            fromImportDeclaration k ns x y
        | InterfaceDeclarationElement(_, x) ->
            fromInterface k ns x

    and fromExternalImportAssignment k ns (ExternalImportDeclaration(x, ExternalModuleReference y)) z =
        k (Import(z, x::ns, [y]))

    and fromExportAssignment k ns (ExportAssignment x) =
        k (Import(true, ns, x::ns))

    and fromAmbientExternalModuleElement k ns = function  
        | ExternalExportAssignment x -> fromExportAssignment k ns x
        | ExternalAbientImportDeclaration(y, x) -> fromExternalImportAssignment k ns x y
        | ExternalAmbientModuleElement x -> fromAmbientModuleElement k ns x 

    and fromAmbientDeclaration k ns = function
        | CommonAmbientElementDeclaration x -> 
            fromCommonAmbientElement k ns x
        | AmbientExternalModuleDeclaration(n, xs) -> 
            xs |> List.iter (fromAmbientExternalModuleElement k (n::ns))
        | AmbientModuleDeclaration(EntityName xs, ys) ->
            ys |> List.iter (fromAmbientModuleElement k ((xs |> List.rev) @ ns))

    and fromDeclarationElement k = function
        | RootExportAssignment _ -> printfn "[WARN] Ignored root level export assignment."
        | RootImportDeclaration _ 
        | RootReference _ -> ()
        | RootExternalImportDeclaration(y, x) -> fromExternalImportAssignment k [] x y
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
            "decimal"; "float32"; "bool"; "obj"; "unit"; "global"; "recursive"; 
            "use"; "let"; "do"; "yield"; "lazy"; "constructor" |]

    let keywords = csharpKeywords + fsharpKeywords

    let sanitise str =
        [   " ", "_"
            "$", "Dollar"
            "-", "_"
            ".", "_"
        ]
        |> List.fold (fun (str : string) (x, y) -> str.Replace(x, y)) str
        |> fun str -> 
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
            elif almostSane = "_" then "Underscore"
            elif keywords.Contains str then "_" + str 
            elif str = "" then "EmptyString"
            else almostSane

    let toLowerCamelCase(str : string) =
        str.[0..0].ToLower() + str.[1..]

    let sanitiseLowerCamelCase = toLowerCamelCase >> sanitise >> toLowerCamelCase

    let sanitiseLiteral str =
        let almostSane =
            str |> String.map (function
                | '\t' | '\r' | '\n' -> ' '
                | c -> c)
        if almostSane = "" then "EmptyString"
        else almostSane

    let fromPropertyName = function
        | NameIdentifier n -> sanitise n, sprintf ".%s" n
        | NameNumericLiteral i -> sprintf "``%i``" i, sprintf "[%i]" i
        | NameStringLiteral s -> 
            // TODO: No idea how this will work in JS if there are illeagal whitespace chars...
            sprintf "``%s``" (sanitiseLiteral s), sprintf "[\\\"%s\\\"]" s

module private Generate =
    
    /// Creates a key based on what F# would consider a unique type.
    /// For example IFoo<'a, 'b> and IFoo<'b, 'c> would clash and
    /// must be merged for this reason.
    let typeRefToTypeKey(TypeRef(ns, ps)) = 
        ns, ps.Length

    let typeRefToDelegateTypeKey = function
        | TypeRef(n::ns, ps) -> Some((n + "Delegate")::ns, ps.Length)
        | _ -> None

    /// Groups ffi elements by the name of the parent type and 
    /// its number of generic parameters.
    let groupByParentType xs =
        xs |> Seq.groupBy (function
            | Import _ -> None
            | Delegate(x, _, _)
            | Enum(x, _)
            | Interface(x, _, _)
            | Member(x, _, _, _) -> Some(typeRefToTypeKey x))
        |> Seq.choose (fun (x, ys) ->
            match x with
            | Some x -> Some(x, set ys)
            | None -> None)
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

    let predefinedTypeKeys =
        set (predefinedTypeRefs |> List.map typeRefToTypeKey)
        |> Set.add (["IList"; "Generic"; "Collections"; "System"], 1)
        |> Set.add (["Array"], 1)
        |> List.foldBack Set.add (List.init 15 (fun i -> (["Func"; "System"], i+1)))

    let predefinedNs =
        predefinedTypeKeys |> Set.map fst

    type NormalisedName = 
    | NormalisedName of string list
    | Empty

    let funscriptNamespace = ["TypeScript";"FunScript" ]

    let normalise ns =
        match ns with
        | [] -> Empty
        | ["Array"] -> NormalisedName ["array"]
        | [genericParam] when genericParam.StartsWith("'") ->
            NormalisedName [Identifier.sanitise genericParam]
        | _ -> 
            let isPredefined = predefinedNs.Contains ns
            let sanitise =
                if isPredefined then id
                else Identifier.sanitise
            let subTypeSpace = ns |> List.map sanitise
            if isPredefined 
            then NormalisedName subTypeSpace 
            else NormalisedName (List.concat [subTypeSpace; funscriptNamespace]) 

    let code = function
        |NormalisedName n -> Some (n |> List.rev |> String.concat ".")
        |Empty -> None

    let nameCode = normalise >> code

    let extractNamespace (TypeRef(n, _)) = 
        match normalise n with 
        | Empty | NormalisedName([]) -> failwith "Expected a valid TypeRef"
        | NormalisedName(_::ns) -> NormalisedName (if ns = [] then funscriptNamespace else ns)

    let namespaceComponentCode n =
        match n |> extractNamespace |> code with
        | None -> "FunScript.TypeScript"
        | Some namespaceCode -> namespaceCode

    let stripNamespace ns nc =
        match ns, nc with
        | (Empty,_) -> nc 
        | (NormalisedName ns, NormalisedName nc) when List.length ns < List.length nc ->
            let _, ln, nspc = List.foldBack 
                                (fun e (n, tn, ns) -> if n > 0 then (n-1, tn, e::ns) else (n, e::tn, ns))
                                nc ( List.length ns, [], [])
            NormalisedName(if ns = nspc then ln else nc )
        | (_,_) -> nc 

    let localNameCode ns tn = tn |> normalise |> stripNamespace ns |> code 

    /// Returns the F# string representing the type ref, relative the the cns namespace.
    /// Adresses issues with the F# compiler not resolving generic and delegate
    /// parameters supplied as full type names when they reference the type being declared
    /// It assumes that the generic parameters have been mapped into F# friendly
    /// back-ticked type refs and that all types have been resolved.
    let rec localTypeReferenceCode cns (TypeRef(ns, gps)) =
        let nameCode = localNameCode cns ns 
        match nameCode, gps with
        | None, _ -> failwith "[ERROR] Cannot emit empty TypeRef reference."
        //| Some "array", [] -> "array<obj>"
        | Some nc, [] -> nc
        | Some nc, gps -> 
                let parameters = gps |> Seq.map (localTypeReferenceCode cns) |> String.concat ", "
                sprintf "%s<%s>" nc parameters

    /// Returns the F# string representing the type ref. It assumes that the generic
    /// parameters have been mapped into F# friendly back-ticked type refs and that
    /// all types have been resolved.
    let typeReferenceCode = localTypeReferenceCode Empty 


    /// Tries to return a (name * namespace * parameter list) tuple for declaring
    /// types. This doesn't search the type space to resolve the type.
    let (|TypeDeclarationId|_|) tr =
        match tr with
        | TypeRef(n::_, gps) -> Some(Identifier.sanitise n, namespaceComponentCode tr, gps)
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
type %s ="""               namespaceId nameId
                
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
    let resolveTypeRef canHaveDelegate typeImports typesByKey (TypeRef(baseNs,_) as baseT) genericTypeParameters t =

        let rec tryReplace acc n =
            match n with
            | [] -> acc |> List.rev
            | _ when n = baseNs -> (acc |> List.rev) @ n
            | x::xs ->
                match typeImports |> List.tryPick (Map.tryFind n) with
                | Some (r : _ list) -> 
                    // TODO: Should we recurse here? What about infinite loops caused
                    //       by imports with longer source sequences?
                    if r.Length <= n.Length then
                        tryReplace acc r
                    else (acc |> List.rev) @ r
                | None -> tryReplace (x::acc) xs 

        let rec tryResolve basePart ((finalPart, paramCount) as x) =
            let typeSpace = finalPart @ basePart 
            let typeSpaces = [typeSpace; tryReplace [] typeSpace] |> Seq.distinct
            typeSpaces |> Seq.tryPick (fun typeSpace ->
                let hasType = 
                    predefinedTypeKeys.Contains (typeSpace, paramCount) ||
                    typesByKey |> List.exists (fun f -> f (typeSpace, paramCount))
                if hasType then Some typeSpace
                else
                    match basePart with
                    | [] -> None
                    | _::subBasePart -> tryResolve subBasePart x)

        t |> TypeRef.map (fun (TypeRef(ns, ps) as t) ->
            if genericTypeParameters |> Set.contains t then t
            else
                match ns, ps with
                | ["Array"],[] -> TypeRef(["Array"], [TypeRef(["obj"], [])]) 
                | ["Array"], [gp] -> TypeRef(["Array"], [gp])
                | ["IList"; "Generic"; "Collections"; "System"], [_]
                | ["Func"; "System"], _ -> t
                | _ ->
                    let keys = [
                        if canHaveDelegate then 
                            match typeRefToDelegateTypeKey t with
                            | None -> ()
                            | Some k -> yield k
                        yield typeRefToTypeKey t
                    ]
                    match keys |> List.tryPick (tryResolve baseNs) with
                    | Some r -> TypeRef(r, ps)
                    | None -> failwithf "[ERROR] Unable to resolve TypeRef: %A (from TypeRef: %A)" t baseT)

    /// This is to work around the restriction where you cannot have a type parameter
    /// that `a :> `b[]
    let replaceArrayWithIList x =
        x |> TypeRef.map (function
            | TypeRef(["Array"], []) -> TypeRef(List.rev ["System"; "Collections"; "Generic"; "IList"], [TypeRef(["obj"], [])])
            | TypeRef(["Array"], ps) -> TypeRef(List.rev ["System"; "Collections"; "Generic"; "IList"], ps)
            | x -> x)

    /// Builds the code segment specifying a sub type relationship type constraint.
    let constraintCode gps (x,y) =
        match x with
        | TypeRef([gn], []) ->
            Some(sprintf "%s :> %s" (typeReferenceCode x) (typeReferenceCode y))
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
            let ncParts = nc.Split [|'.'|]
            let localNc = ncParts.[ncParts.Length - 1]
            match constrainedGenericParameterCode gps gcs with
            | None -> nc, localNc
            | Some genericDef -> nc + genericDef, localNc + genericDef

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

    let delegateCode typeImports typesByKey (TypeRef(ns, gps) as t) ps r =
        try
            let fixedGps, fixedGpsSet, fix = fixGenericParameters gps
            let fix = fix >> resolveTypeRef true typeImports typesByKey t fixedGpsSet
            let fixedPs = ps |> List.map fix
            let fixedR = fix r
            let dependencies = 
                let gpsKeys = fixedGpsSet |> Set.map typeRefToTypeKey
                let expandedDeps =
                    [yield fixedR; yield! fixedPs] 
                    |> Seq.collect (TypeRef.collect typeRefToTypeKey) 
                    |> set
                (expandedDeps - gpsKeys) |> Set.remove (typeRefToTypeKey t)
            let delegateSignature, localSignature =
                constrainedTypeReferenceCode (TypeRef(ns, fixedGps)) []
            let localTypeCode = extractNamespace t |> localTypeReferenceCode
            let parameterCode = 
                match fixedPs with
                | [] -> "unit"
                | _ -> fixedPs |> Seq.map localTypeCode |> String.concat " * "
            let returnCode = localTypeCode fixedR
            let delegateDeclaration =
                sprintf """
namespace %s
type %s = delegate of %s -> %s
"""                 (namespaceComponentCode t) localSignature parameterCode returnCode
            Some(delegateDeclaration,
                 dependencies,
                 typeRefToTypeKey t, 
                 Some(delegateSignature, fixedGps))
        with ex ->
            printfn "[ERROR] %s" ex.Message
            None

    let interfaceCode typeImports typesByKey (TypeRef(ns, gps) as t) gcs sts =
        try
            let fixedGps, fixedGpsSet, fix = fixGenericParameters gps
            let fixImport = fix >> resolveTypeRef false typeImports typesByKey t fixedGpsSet
            let fix = fix >> resolveTypeRef true typeImports typesByKey t fixedGpsSet
            let fixedGcs = gcs |> List.map (fun (x, y) -> fix x, fix(replaceArrayWithIList y))
            let fixedSts = sts |> List.map (replaceArrayWithIList >> fixImport) |> Seq.distinct |> Seq.toList
            let dependencies = 
                let gpsKeys = fixedGpsSet |> Set.map typeRefToTypeKey
                let possibleDeps = fixedSts @ (fixedGcs |> List.map snd)
                let expandedDeps = possibleDeps |> Seq.collect (TypeRef.collect typeRefToTypeKey) |> set
                (expandedDeps - gpsKeys) |> Set.remove (typeRefToTypeKey t)
            let interfaceSignature, localSignature =
                constrainedTypeReferenceCode (TypeRef(ns, fixedGps)) fixedGcs
            //TypeRef(ns, fixedGps), fixedGcs, fixedSts, interfaceSignature

            let interfaceDeclaration =
                sprintf """
namespace %s
type %s ="""        (namespaceComponentCode t) localSignature
            let interfaceBodyEls =
                fixedSts |> List.map (extractNamespace t |> localTypeReferenceCode)
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
            printfn "[ERROR] %s" ex.Message
            None
        

    /// Returns the code of the type declarations needed for a given
    /// module of FFI elements. This filters out the types already
    /// declared in dependencies. Also, returns a map from type key
    /// to type signature.
    let typeDeclarationsAndSignatures (moduleElements : Map<_,_>) typeImports typesByKey =
        let typesByKeyInclCurrentModule =
            moduleElements.ContainsKey :: typesByKey

        moduleElements |> Map.toSeq |> Seq.choose (fun (typeKey, els) ->
            let wasAlreadyDeclared = typesByKey |> List.exists (fun f -> f typeKey)
            if wasAlreadyDeclared then None
            else 
                let typeFound =
                    els |> Seq.tryPick (function
                        | Interface(TypeRef(["Array"], _) as t, gcs, sts)
                        | Interface(TypeRef(["array"], _) as t, gcs, sts) ->
                            interfaceCode typeImports typesByKeyInclCurrentModule t gcs sts
                            |> Option.map (fun (c, x, y, z) -> "", x, y, z)
                        | Interface(t, gcs, sts) -> interfaceCode typeImports typesByKeyInclCurrentModule t gcs sts
                        | Enum(t, []) -> interfaceCode typeImports typesByKeyInclCurrentModule t [] []
                        | Enum(t, ms) -> enumCode t ms
                        | Delegate(t, ps, r) -> delegateCode typeImports typesByKeyInclCurrentModule t ps r
                        | Member _ | Import _ -> None)
                match typeFound with
                | None -> interfaceCode typeImports typesByKeyInclCurrentModule (TypeRef("Globals" :: fst typeKey, [])) [] []
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
        | Variable(name, t, opt) -> Variable(name, fix t, opt)
        | InlineArray(name, t) -> InlineArray(name, fix t)
        | Fixed _ as x -> x

    let parameterCode = function
        | Variable(name, t, Required) -> 
            "", Some(sprintf "%s : %s" (Identifier.sanitiseLowerCamelCase name) (typeReferenceCode t))
        | Variable(name, t, Optional) ->
            "", Some(sprintf "?%s : %s" (Identifier.sanitiseLowerCamelCase name) (typeReferenceCode t))
        | InlineArray(name, t) -> 
            "", Some(sprintf "[<System.ParamArray>] %s : %s" (Identifier.sanitiseLowerCamelCase name) (typeReferenceCode t))
        | Fixed v -> 
            let specialization = 
                let n = Identifier.sanitise v
                if n.StartsWith "_" then n
                else "_" + n
            specialization, None

    let javaScriptTypeCode (TypeRef(ns, _)) =
        match ns with
        | [] -> "window"
        | _ -> ns |> List.rev |> String.concat "."

    /// See: http://stackoverflow.com/questions/19516961/why-cant-you-use-generic-type-parameters-in-constraints-on-type-extensions
    let bugFixTypeExtensionGenericConstraint fixedGpsSet fixedMethodGps fixedGcs =
        let notAllowableGcs, allowableGcs =
            fixedGcs |> List.partition (fun (x, y) ->
                y |> TypeRef.exists (fun z -> fixedGpsSet |> Set.contains z))
        let notAllowableMap = notAllowableGcs |> Map.ofList
        let fix = TypeRef.map (fun x -> defaultArg (notAllowableMap.TryFind x) x)
// TOO STRICT: Need to take into account parameters and return type too!
//        let allowableGps = 
//            fixedMethodGps |> List.filter (fun x -> 
//                allowableGcs |> List.exists (fun (y, z) ->
//                    y |> TypeRef.exists ((=) x) ||
//                    z |> TypeRef.exists ((=) x)))
        let allowableGps = fixedMethodGps
        fix, allowableGps, allowableGcs

    /// Fast compiled name generation using mutable Dictionary:
    let usedNamesMap = new Dictionary<string, int>()
    let compiledName name = 
        let i = ref -1
        let found = usedNamesMap.TryGetValue(name, i)
        usedNamesMap.[name] <- !i + 1
        if found then sprintf "%s_%i" name !i else name

    let typeExtensionCode typeImports isMemberDefined typesByKey ns signature fixedGps members =
        let fixedGpsSet = set fixedGps

        let membersCode =
            members |> Seq.choose (fun (TypeRef(_, gps) as t, pn, s, el) ->
                try
                    let fixMap = List.zip gps fixedGps |> Map.ofList
                    let fixGenericTypeParameters = 
                        TypeRef.map (fun x -> defaultArg (fixMap.TryFind x) x)
                    let fixAndResolve = 
                        fixGenericTypeParameters >> resolveTypeRef true typeImports typesByKey t fixedGpsSet
                    let fixedT = fixGenericTypeParameters t
                    let name, accessCode, prelude = 
                        match pn with
                        | Indexer -> "Item", "", ""
                        | Invoker -> "Invoke", "", ""
                        | Constructor -> "Create", "", "new "
                        | Name pn -> 
                            let n, a = Identifier.fromPropertyName pn
                            n, a, ""
                    let compiledName = compiledName name
                    let baseAccess, seedI =
                        match s with
                        | Static -> javaScriptTypeCode t, 0
                        | NonStatic -> "{0}", 1
                    let fullAccess = prelude + baseAccess + accessCode
                    let callCode =
                        match el with
                        | Property _ -> fullAccess
                        | Method(_, _, ps, _) -> 
                            let parameterCode =
                                ps |> List.fold (fun (i, acc) -> function
                                    | Fixed v -> i, sprintf "\\\"%s\\\"" v :: acc
                                    | Variable(_,_,Required) -> i + 1, sprintf "{%i}" i :: acc
                                    | Variable(_,_,Optional) -> i + 1, sprintf "{?%i}" i :: acc
                                    | InlineArray _ -> i + 1, sprintf "{%i...}" i :: acc) (seedI, [])
                                |> snd |> List.rev |> String.concat ", "
                            sprintf "%s(%s)" fullAccess parameterCode
                        | Index _ ->
                            sprintf "%s[{%i}]" fullAccess seedI
                        | FunWrapper ->
                            let n = (List.length gps - 1)
                            let args = [ for i in 0..n-2 -> sprintf "x%d" i ] 
                            sprintf "function(%s) { return {0}%s([].slice.call(arguments, %i)); }" 
                                (args |> String.concat ", ")
                                (args |> List.map (sprintf "(%s)") |> String.concat "")
                                (n-1)
                    let getOverloadName specializedName memberElement =
                        let memberElementKey =
                            match memberElement with
                            | Property _ | Index _ | FunWrapper -> memberElement
                            | Method(gps, gcs, ps, r) ->
                                let requiredParameters =
                                    ps |> List.choose (function
                                        | Fixed _ | InlineArray _ -> None
                                        | Variable(_,pt,optionality) -> 
                                            match optionality with
                                            | Optional -> None
                                            | Required -> Some(Variable("", pt, Required)))
                                Method(gps, gcs, requiredParameters, r)
                        Seq.initInfinite id |> Seq.map (function
                            | 0 -> specializedName
                            | n -> sprintf "%sOverload%i" specializedName (n+1))
                        |> Seq.find (fun name ->
                            not (isMemberDefined (signature, name, s, memberElementKey)))
                    let memberSignature, auxMemberSignature =
                        match el with
                        | Property r -> 
                            let fixedR = fixAndResolve r
                            let rCode = typeReferenceCode fixedR
                            let name = getOverloadName name (Property fixedR)
                            sprintf "%s with get() : %s = failwith \"never\" and set (v : %s) : unit = failwith \"never\"" name rCode rCode,
                            None
                        | Method(methodGps, methodGcs, ps, r) ->
                            let partiallyFixedMethodGps, _, fixGenericNames = fixGenericParameters methodGps
                            let collisionFix = fixGenericNames >> collisionFix fixedGpsSet partiallyFixedMethodGps
                            let fixedMethodGps = methodGps |> List.map collisionFix
                            let allGps = set fixedMethodGps + fixedGpsSet
                            let combinedFix = collisionFix >> fixGenericTypeParameters >> resolveTypeRef true typeImports typesByKey t allGps
                            let fixedGcs = methodGcs |> List.map (fun (x, y) -> combinedFix x, combinedFix y)
                            let allowableFix, fixedMethodGps, fixedGcs = 
                                bugFixTypeExtensionGenericConstraint fixedGpsSet fixedMethodGps fixedGcs
                            let combinedFix = combinedFix >> allowableFix
                            let fixedPs = ps |> List.map (mapParameterType combinedFix)
                            let specialization, parametersCode = 
                                let specs, pCodes = fixedPs |> List.map parameterCode |> List.unzip
                                specs |> String.concat "", pCodes |> List.choose id |> String.concat ", "
                            let fixedR = combinedFix r
                            let genericDef = defaultArg (constrainedGenericParameterCode fixedMethodGps fixedGcs) ""
                            let specializedName = name + specialization
                            let specializedName = getOverloadName specializedName (Method(fixedMethodGps, fixedGcs, fixedPs, fixedR))
                            let canHaveSetter = 
                                let hasSpecialArgs =
                                    fixedPs |> List.exists (function
                                        | Fixed _ | InlineArray _ -> true
                                        | Variable _ -> false)
                                fixedPs.Length < 9 &&
                                not hasSpecialArgs
                            let auxMethod =
                                if canHaveSetter then
                                    let auxName = sprintf "``%s <-``" (specializedName.Trim [|'`'|])
                                    let auxArgs = 
                                        fixedPs |> List.map (function
                                            | Variable(_, t, _) -> t
                                            | _ -> failwith "never")
                                    let paramT = TypeRef(["Func"; "System"], [yield! auxArgs; yield fixedR])
                                    let paramCode = typeReferenceCode paramT
                                    Some(sprintf "%s%s(func : %s) : unit = failwith \"never\"" auxName genericDef paramCode,
                                         sprintf "%s = {%i}" fullAccess seedI)
                                else None
                            sprintf "%s%s(%s) : %s = failwith \"never\"" 
                                specializedName genericDef parametersCode (typeReferenceCode fixedR),
                            auxMethod
                        | Index(p, r) ->
                            let fixedP = fixAndResolve p
                            let fixedR = fixAndResolve r
                            let pCode = typeReferenceCode fixedP
                            let rCode = typeReferenceCode fixedR
                            let name = getOverloadName name (Index(fixedP, fixedR))
                            sprintf "%s with get(i : %s) : %s = failwith \"never\" and set (i : %s) (v : %s) : unit = failwith \"never\"" name pCode rCode pCode rCode,
                            None
                        | FunWrapper ->
                            let n = List.length gps
                            let tpar (TypeRef([s],ts)) = s
                            let gparams = gps |> List.map (fixGenericParameterName >> tpar)
                            let args = Seq.take (n-2) gparams |> Seq.map (sprintf "%s -> ") |> String.concat ""
                            sprintf "Create(f: %s%s array -> %s) : %s = failwith \"never\"" 
                                args 
                                (List.nth gparams (n-2)) 
                                (List.nth gparams (n-1)) 
                                (Option.get signature),
                            None
                    let root =
                        match s with
                        | Static -> "static member "
                        | NonStatic -> "member __."
                    sprintf """
            [<FunScript.JSEmitInline("(%s)"); CompiledName("%s")>]
            %s%s"""                 callCode compiledName root memberSignature
                    |> Option.foldBack (fun (auxSignature, auxCallCode) acc  ->
                        sprintf """%s
            [<FunScript.JSEmitInline("(%s)"); CompiledName("%sAux")>]
            %s%s"""                 acc auxCallCode compiledName root auxSignature)
                        auxMemberSignature
                    |> Some
                with ex ->
                    printfn "[ERROR] %s" ex.Message
                    None
                ) 
            |> String.concat ""

        let namespaceCode =
            match signature, ns with
            | _, [] -> None
            | Some _, _::ns -> nameCode ns
            | None, _ -> nameCode ns

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
                    sprintf "    type System.Collections.Generic.IList<%s> with " parts.[0]
                else
                    sprintf "    type %s with " signature

        if membersCode = "" then None
        else
            Some (
                System.Environment.NewLine +
                typeExtensionDefinitionCode + System.Environment.NewLine +
                membersCode + System.Environment.NewLine)


    let typeExtensions moduleName typeImports isMemberDefined signaturesByKey moduleElements =
        // TODO: Perhaps choose namespace based on type extensions. However, this may reduce discoverability.
        let precursor i = sprintf """
namespace FunScript.TypeScript

[<AutoOpen>]
module TypeExtensions_%s_%i =

"""                         moduleName i
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
                match signaturesByKey |> List.tryPick (Map.tryFind typeKey) |> Option.bind id with
                | None -> typeExtensionCode typeImports isMemberDefined signaturesByKeyTest ns None [] members
                | Some(signature, fixedGps) -> typeExtensionCode typeImports isMemberDefined signaturesByKeyTest ns (Some signature) fixedGps members
            )
        |> Seq.mapi (fun i x -> i, x)
        |> Seq.fold (fun (acc : StringBuilder) (i, code) -> acc.Append(precursor i).Append(code)) (StringBuilder())
        |> fun sb -> sb.ToString()

module Compiler =

    let findModuleDependencies (moduleName, modulePath, DeclarationsFile decls) =
        let moduleDir = System.IO.Path.GetDirectoryName modulePath
        let isLib =
            decls |> List.exists (function
                | RootReference(NoDefaultLib true) -> true
                | _ -> false)
        let rest =
            decls |> List.choose (function
                | RootReference(File path) -> 
                    let refPath = path.Replace('/', '\\')
                    let partRelativePath = System.IO.Path.Combine(moduleDir, refPath)
                    let absolutePath = System.IO.Path.GetFullPath partRelativePath
                    Some absolutePath
                | _ -> None)
        if isLib then rest
        else "lib.d.ts" :: rest
                
    let orderByCompilationDependencies declarationModules =
        declarationModules |> List.topologicalSortBy 
            (fun (name, path, decls) -> path) findModuleDependencies

    let private findAndConvertDelegates hasPriorDefinition elementsByNameKey =
        elementsByNameKey |> Seq.collect (fun (k, els) ->
            match els with
            | [Interface(_, [], []); Member(TypeRef(_,gps) as t, Invoker, NonStatic, Method([], [], ps, r))]
            | [Member(TypeRef(_,gps) as t, Invoker, NonStatic, Method([], [], ps, r)); Interface(_, [], [])]
                when ps |> List.forall (function Variable _ -> true | _ -> false) &&
                        not (hasPriorDefinition  k) ->
                let args = ps |> List.map (function Variable(_, t, _) -> t | _ -> failwith "never")
                match Generate.typeRefToDelegateTypeKey t with
                | Some dk ->
                    [
                        dk, [Delegate(TypeRef(fst dk, gps), args, r)]
                        k, els
                    ]
                | None -> [k, els]
            | _ -> [k, els]
        )

    let generateUniqueNames declarationFiles=
        declarationFiles |> List.fold (fun namesToDecls (path : string, _, decls) ->
            let nameParts = path.Substring(0, path.Length - ".d.ts".Length).Split('/','\\')
            nameParts |> Array.rev |> Array.map (fun str -> str.ToLowerInvariant())
            |> Array.scan (fun acc namePart -> 
                match acc with
                | None -> Some namePart
                | Some prevPart -> Some(sprintf "%s.%s" namePart prevPart)) None
            |> Seq.choose (Option.map Identifier.sanitise)
            |> Seq.tryPick (fun chosenName ->
                match namesToDecls |> Map.tryFind chosenName with
                | None -> Some(namesToDecls |> Map.add chosenName (path, decls))
                | Some _ -> None)
            |> function
                | Some acc -> acc
                | None -> 
                    // TODO: Could just append numbers here
                    failwithf "No unique name found for: %s" path) Map.empty
        |> Map.toList
        |> List.map (fun (name, (path, decls)) -> name, path, decls)

    let generateTypes declarationFiles =

        let modules = generateUniqueNames declarationFiles
            
        printfn "Ordering modules..."

        let lines = modules |> List.toArray |> Array.map (fun (moduleName, _,_) -> moduleName)
        System.IO.File.WriteAllLines("uniqueModuleNames.txt", lines)

        let modulesInCompilationOrder =
            orderByCompilationDependencies modules
            |> Seq.toList

        let lines = 
            modulesInCompilationOrder 
            |> List.map (fun (_, modulePath, _) -> modulePath)
            |> List.toArray
        System.IO.File.WriteAllLines("compilationOrder.txt", lines)

        let modulePathToName =
            modules |> List.map (fun (moduleName, modulePath, _) -> 
                modulePath, moduleName)
            |> Map.ofList

        let dependencyMap =
            modulesInCompilationOrder 
            |> Seq.map (fun (_, x, xs) -> x, xs)
            |> Map.ofSeq

        let lines = dependencyMap |> Map.toArray |> Array.map (sprintf "%A")
        System.IO.File.WriteAllLines("dependencyMap.txt", lines)

        let findAllDependencies modulePath =
            let rec findAll acc names =
                match names with
                | [] -> acc
                | _ -> 
                    let nextNames = names |> Seq.choose dependencyMap.TryFind |> Seq.concat |> Seq.toList
                    findAll (names @ acc) nextNames
            findAll [] (dependencyMap.TryFind modulePath |> Option.toList |> List.concat)
            |> Seq.distinct |> Seq.toList

        let codeByModule =
            modulesInCompilationOrder
            |> List.fold (fun (codeOut, moduleTypes, typeImports, membersDefined) ((moduleName, _, DeclarationsFile decls), modulePath, _) ->
                let moduleDependencies = findAllDependencies modulePath
                let xs = ResizeArray()
                decls |> List.iter (FFIMapping.fromDeclarationElement xs.Add)
                // TODO: This doesn't protect against same signatures in the same module.
                let allMembersSoFar =
                    moduleDependencies |> List.fold (fun acc n -> acc + (membersDefined |> Map.find n)) Set.empty
                let localMembersDefined = ref Set.empty
                let isMemberDefined memberKey =
                    let isDefined =
                        allMembersSoFar |> Set.contains memberKey ||
                        !localMembersDefined |> Set.contains memberKey
                    if not isDefined then
                        localMembersDefined := !localMembersDefined |> Set.add memberKey
                    isDefined
                let dependencySigs =
                    moduleDependencies |> List.map (fun n -> moduleTypes |> Map.find n)
                let typesByKey = dependencySigs |> List.map (fun (x : Map<_,_>) -> x.ContainsKey)
                let elementsByNameKey = 
                    xs |> Seq.groupBy (function
                        | Import _ -> None
                        | Delegate(t, _, _)
                        | Interface(t, _, _) 
                        | Enum(t, _)
                        | Member(t, _, _, _) -> Some(Generate.typeRefToTypeKey t))
                    |> Seq.choose (fun (k, vs) -> k |> Option.map (fun k -> k, vs |> Seq.toList))
                    |> findAndConvertDelegates (fun k -> typesByKey |> List.exists (fun f -> f k))
                    |> Map.ofSeq
                let exportedTypeImports =
                    xs |> Seq.choose (function
                        // TODO: For now we are assuming everything is exported...
                        //       ...but really we should filter and treat them differently.
                        | Import(_, x, y) when x <> y -> Some(x, y)
                        | _ -> None)
                    |> Map.ofSeq
                let allTypeImports =
                    exportedTypeImports ::
                    (moduleDependencies |> List.map (fun n -> typeImports |> Map.find n))
                printfn "Generating type signatures for %s..." moduleName
                let declarationCode, sigs = 
                    Generate.typeDeclarationsAndSignatures elementsByNameKey allTypeImports typesByKey
                printfn "Generating type extensions for %s..." moduleName
                let extensionCode =
                    Generate.typeExtensions moduleName allTypeImports isMemberDefined (sigs :: dependencySigs) elementsByNameKey
                let code = declarationCode + System.Environment.NewLine + extensionCode
                codeOut |> Map.add moduleName code, 
                moduleTypes |> Map.add modulePath sigs,
                typeImports |> Map.add modulePath exportedTypeImports,
                membersDefined |> Map.add modulePath !localMembersDefined
            ) (Map.empty, Map.empty, Map.empty, Map.empty)
            |> fun (x, _, _, _) -> x

        modulesInCompilationOrder |> List.map (fun ((name,_,_), path, _) ->
            name, codeByModule |> Map.find name, findAllDependencies path |> List.choose modulePathToName.TryFind)