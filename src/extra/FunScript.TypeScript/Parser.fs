#if INTERACTIVE
#r "..\ThirdParty\FParsecCS.dll"
#r "..\ThirdParty\FParsec.dll"
#else
module internal FunScript.TypeScript.Parser
open AST
#endif

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let str = pstring

let stringLiteralBetween braceChar =
    let escape =  
        anyOf [yield braceChar; yield! "\\/bfnrt"]
        |>> function
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c   -> string c

    let unicodeEscape =
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> braceChar && c <> '\\')


    between (str (braceChar.ToString())) (str (braceChar.ToString()))
            (stringsSepBy normalCharSnippet escapedCharSnippet)

let stringLiteralAux =
    choice [
        stringLiteralBetween '"'
        stringLiteralBetween '\''
    ]

let singlelineComment =
    attempt(skipString "//" >>. notFollowedBy (skipString "/" >>. spaces >>. skipString "<") >>. skipRestOfLine true)

//TODO: Nested comments...
let multilineComment =
    skipString "/*" >>. skipCharsTillString "*/" true System.Int32.MaxValue

let choice_attempt ps =
    choice (ps |> Seq.map attempt)

let ws = 
    choice [
        spaces1
        // Non-breaking space: &nbsp; char code #160
        charReturn ' ' ()
        multilineComment
        singlelineComment
    ] |> skipMany

let ws1 = 
    choice [
        spaces1
        // Non-breaking space: &nbsp; char code #160
        charReturn ' ' ()
        multilineComment
        singlelineComment
    ] |> skipMany1

let str_ws s = pstring s .>> ws
let stringReturn_ws s x = stringReturn s x .>> ws
let stringReturn_ws1 s x = attempt(stringReturn s x .>> ws1)

let stringLiteral = stringLiteralAux .>> ws

let pbool_ws =
    choice [
        stringReturn_ws "true" true
        stringReturn_ws "false" false
    ] .>> ws

let pfloat_ws = 
    pfloat .>> ws

let pint32_ws =
    pint32 .>> ws

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_' || c = '$'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '$'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" 
    .>> ws

let listBetweenStrings sOpen sClose sSep pElement =
    between (str_ws sOpen) (str_ws sClose)
            // TODO: split out cases for sepBy and sepEndBy
            (sepEndBy (pElement .>> ws) (str_ws sSep))
    .>> ws

let sepByComma1 pElement =
    sepBy1 pElement (str_ws ",")

let sepEndByUntil pElement sepStr pUntil =
    many1Till (pElement .>> str_ws sepStr) (followedBy pUntil)

let typeReference, typeReferenceRef = createParserForwardedToRef()
let objectType, objectTypeRef = createParserForwardedToRef()
let typeSpec, typeSpecRef = createParserForwardedToRef()

let extendsClause =
    choice [
        str_ws "extends" >>. typeSpec |>> Some
        preturn None
    ]

let extendsReferenceClause =
    choice [
        str_ws "extends" >>. typeReference |>> Some
        preturn None
    ]

let typeParam =
    identifier .>>. extendsClause |>> TypeParameter

let typeParams =
    choice [
        listBetweenStrings "<" ">" "," typeParam
        preturn []
    ]

let typeArgs =
    choice [
        listBetweenStrings "<" ">" "," typeSpec
        preturn []
    ]

let entityName =
    sepBy1 identifier (str_ws ".")
    |>> EntityName

do  typeReferenceRef := entityName .>>. typeArgs |>> TypeReference

let interfaceExtendsClause =
    choice [
        str_ws "extends" >>. sepBy typeReference (str_ws ",")
        preturn []
    ] |>> InterfaceExtendsClause

let propertyName =
    choice [
        pint32 |>> NameNumericLiteral
        stringLiteral |>> NameStringLiteral
        identifier |>> NameIdentifier
    ]

let optionality =
    choice [
        str_ws "?" >>. preturn true
        preturn false
    ]

let predefinedType =
    choice [
        stringReturn_ws "any" Any
        stringReturn_ws "number" Number
        stringReturn_ws "boolean" Boolean
        stringReturn_ws "string" String
        stringReturn_ws "void" Void
    ]

let typeAnnotation =
    str_ws ":" >>. typeSpec |>> TypeAnnotation

let publicOrPrivate =
    choice [
        stringReturn_ws1 "public" Public
        stringReturn_ws1 "private" Private
    ]

let requiredParameter =
    choice_attempt [
        identifier .>>. (str_ws ":" >>. stringLiteral) |>> StringEnumParameter

        opt publicOrPrivate .>>. identifier .>>. opt typeAnnotation
        |>> (fun ((pp, id), t) -> VariableParameter(pp, id, t))
    ]
    

let literalValue =
    choice [
        pbool_ws |>> BooleanValue
        pfloat_ws |>> NumberValue
        stringLiteral |>> StringValue
        stringReturn_ws "null" NullValue
        stringReturn_ws "undefined" NullValue
        identifier |>> UnknownValue
    ]

let initialiser =
    str_ws "=" >>. literalValue

let optionalParameter =
    choice_attempt [
        opt publicOrPrivate .>>. identifier .>> str_ws "?"
        .>>. opt typeAnnotation
        |>> (fun ((pp, id), t) -> OptionalParameter(pp, id, t))
        
        /// ***** NOTE: NOT DECLARED IN SPEC BUT USED AND ACCEPTED BY COMPILER
        opt publicOrPrivate .>>. identifier .>> str_ws "?" .>> str_ws ":"
        .>>. stringLiteral
        |>> (fun ((pp, id), _) -> OptionalParameter(pp, id, Some(TypeAnnotation(Predefined String))))

        opt publicOrPrivate .>>. identifier
        .>>. opt typeAnnotation
        .>>. initialiser
        |>> (fun (((pp, id), t), v) -> DefaultParameter(pp, id, t, v))
    ]

let restParameter =
    str_ws "..." >>. identifier .>>. opt typeAnnotation
    |>> RestParameter

let paramList =
    between (str_ws "(") (str_ws ")") 
        (choice_attempt [
            
            sepEndByUntil requiredParameter "," optionalParameter
            .>>. sepEndByUntil optionalParameter "," restParameter
            .>>. restParameter
            |>> (fun ((rps, ops), r) -> ParameterList(rps, ops, Some r))

            sepEndByUntil optionalParameter "," restParameter
            .>>. restParameter
            |>> (fun (ops, r) -> ParameterList([], ops, Some r))

            sepEndByUntil requiredParameter "," restParameter
            .>>. restParameter
            |>> (fun (rps, r) -> ParameterList(rps, [], Some r))

            restParameter |>> (fun r -> ParameterList([], [], Some r))

            sepEndByUntil requiredParameter "," optionalParameter
            .>>. sepByComma1 optionalParameter
            |>> (fun (rps, ops) -> ParameterList(rps, ops, None))

            sepByComma1 optionalParameter
            |>> (fun ops -> ParameterList([], ops, None))

            sepByComma1 requiredParameter
            |>> (fun rps -> ParameterList(rps, [], None))

            preturn (ParameterList([], [], None))
        ])

let objectLiteral = objectType |>> LiteralObjectType

let constructorLiteral = 
    str_ws "new" >>. typeParams .>>. paramList .>>. (str_ws "=>" >>. typeSpec)
    |>> (fun ((tps, ps), rt) -> LiteralConstructorType(tps, ps, rt))

let functionLiteral =
    typeParams .>>. paramList .>>. (str_ws "=>" >>. typeSpec)
    |>> (fun ((tps, ps), rt) -> LiteralFunctionType(tps, ps, rt))

let followedByArraySig pSpec =
    pSpec .>> str_ws "[" .>> str_ws "]" |>> LiteralArrayType

let typeQuery =
    str_ws "typeof" >>. entityName |>> TypeQuery

let typeLiteralPrime, typeLiteralPrimeRef = createParserForwardedToRef()

do  typeLiteralPrimeRef :=
        choice_attempt [
            str_ws "[" .>> str_ws "]" >>. typeLiteralPrime |>> (fun g -> fun x -> LiteralArrayType(Literal(g x)))
            //typeLiteral |>> Literal |> followedByArraySig
            preturn id
        ]

/// Note: rearranged due to indirect left recursion!
let typeLiteral =
    let prime pElement = pElement .>>. typeLiteralPrime |>> (fun (x, f) -> f x)
    choice_attempt [
        objectLiteral |> prime
        constructorLiteral |> prime
        functionLiteral |> prime
        predefinedType |>> Predefined |> followedByArraySig |> prime
        typeReference |>> Reference |> followedByArraySig |> prime
        typeQuery |>> Query |> followedByArraySig |> prime
    ]

/// Note: rearranged due to indirect left recursion!
do  typeSpecRef :=
        choice_attempt [
            typeLiteral |>> Literal
            typeQuery |>> Query
            predefinedType |>> Predefined
            typeReference |>> Reference
        ]



let propertySignature =
    propertyName .>>. optionality .>>. opt typeAnnotation
    |>> (fun ((n, o), t) -> PropertySignature(n, o, t))

let callSignature =
    typeParams .>>. paramList .>>. opt typeAnnotation
    |>> (fun ((n, ps), t) -> CallSignature(n, ps, t))

let constructSignature =
    str_ws "new" >>. typeParams .>>. paramList .>>. opt typeAnnotation
    |>> (fun ((n, ps), t) -> ConstructSignature(n, ps, t))

let indexSignature =
    choice_attempt [
        str_ws "[" >>. identifier .>> str_ws ":" .>> str_ws "string" 
        .>> str_ws "]" .>>. typeAnnotation
        |>> IndexSignatureString

        str_ws "[" >>. identifier .>> str_ws ":" .>> str_ws "number" 
        .>> str_ws "]" .>>. typeAnnotation
        |>> IndexSignatureNumber
    ]

let methodSignature =
    propertyName .>>. optionality .>>. callSignature
    |>> (fun ((n, o), cs) -> MethodSignature(n, o, cs))

let typeMember =
    choice_attempt [
        constructSignature |>> MemberConstructSignature
        methodSignature |>> MemberMethodSignature
        propertySignature |>> MemberPropertySignature
        callSignature |>> MemberCallSignature
        indexSignature |>> MemberIndexSignature
    ]

do 
    objectTypeRef :=
        between (str_ws "{") (str_ws "}") 
            (many (typeMember .>> opt(str_ws ";"))) |>> ObjectType

let exportAssignment =
    str_ws "export" >>. str_ws "=" >>. identifier
    |>> ExportAssignment

let interfaceDeclaration =
    str_ws "interface" >>. identifier .>>. typeParams 
    .>>. interfaceExtendsClause .>>. objectType 
    |>> (fun (((id, tps), ex), t) -> InterfaceDeclaration(id, tps, ex, t))

let importDeclaration =
    str_ws "import" >>. identifier .>>. entityName
    |>> ImportDeclaration

let externalModuleReference =
    str_ws "require" >>. choice [
        str_ws "(" >>. stringLiteral .>> str_ws ")"
        stringLiteral 
    ]
    |>> ExternalModuleReference

let externalImportDeclaration =
    str_ws "import" >>. identifier .>>. (str_ws "=" >>. externalModuleReference)
    |>> ExternalImportDeclaration

let implementsClause =
    choice [
        str_ws "implements" >>. sepBy typeReference (str_ws ",")
        preturn []
    ]

let classHeritage =
    extendsReferenceClause .>>. implementsClause
    |>> ClassHeritage
    
let staticness =
    choice [
        stringReturn_ws "static" true
        preturn false
    ]

let ambientMemberDeclaration =
    choice_attempt [
        opt publicOrPrivate .>>. staticness .>>. propertyName .>>. callSignature
        |>> (fun (((pp, s), n), c) -> AmbientMethodDeclaration(pp, s, n, c))

        opt publicOrPrivate .>>. staticness .>>. propertyName .>>. opt typeAnnotation
        |>> (fun (((pp, s), n), t) -> AmbientPropertyDeclaration(pp, s, n, t))

        opt publicOrPrivate .>> stringReturn_ws "static" true .>>. callSignature
        |>> (fun (pp, c) -> AmbientMethodDeclaration(pp, false, NameIdentifier "static", c))
    ]

let ambientClassBodyElement =
    choice [
        str_ws "constructor" >>. paramList
        |>> AmbientConstructorDeclaration

        ambientMemberDeclaration |>> AmbientMemberDeclaration

        indexSignature |>> AmbientIndexSignature
    ]

let ambientEnumMember =
    choice_attempt [
        propertyName .>>. (str_ws "=" >>. pint32_ws) |>> NumberedCase

        propertyName |>> NamedCase
    ]

let commonAmbientElementDeclaration =
    choice [
        str_ws "var" >>. identifier .>>. opt typeAnnotation .>> opt (str_ws ";")
        |>> AmbientVariableDeclaration

        str_ws "function" >>. identifier .>>. callSignature .>> opt (str_ws ";")
        |>> AmbientFunctionDeclaration

        str_ws "class" >>. identifier .>>. typeParams .>>. classHeritage
        .>>. (between (str_ws "{") (str_ws "}") (many (ambientClassBodyElement .>> opt (str_ws ";")))) .>> opt (str_ws ";")
        |>> (fun (((id, tps), ch), els) -> AmbientClassDeclaration(id, tps, ch, els))

        str_ws "enum" >>. identifier
        .>>. listBetweenStrings "{" "}" "," ambientEnumMember .>> opt (str_ws ";")
        |>> AmbientEnumDeclaration
    ]


let exportedness =
    choice [
        stringReturn_ws "export" true
        preturn false
    ]

let ambientModuleElement, ambientModuleElementRef = createParserForwardedToRef()

let ambientModuleDeclaration =
    str_ws "module" >>. entityName 
    .>>. between (str_ws "{") (str_ws "}") (many ambientModuleElement)
    

do  ambientModuleElementRef :=
    choice_attempt [
        exportedness .>>. commonAmbientElementDeclaration
        |>> AmbientModuleElement

        exportedness .>>. interfaceDeclaration .>> opt (str_ws ";")
        |>> InterfaceDeclarationElement

        exportedness .>>. ambientModuleDeclaration .>> opt (str_ws ";")
        |>> (fun (e, (n, els)) -> AmbientModuleDeclarationElement(e, n, els))

        exportedness .>>. importDeclaration .>> opt (str_ws ";")
        |>> ImportDeclarationElement
    ]

let ambientExternalModuleElement =
    choice_attempt [
        ambientModuleElement |>> ExternalAmbientModuleElement
        exportAssignment .>> opt (str_ws ";") |>> ExternalExportAssignment
        exportedness .>>. externalImportDeclaration .>> opt (str_ws ";")
        |>> ExternalAbientImportDeclaration
    ]

let ambientDeclaration =
    choice_attempt [

        str_ws "declare" >>. commonAmbientElementDeclaration
        |>> CommonAmbientElementDeclaration
        
        str_ws "declare" >>. ambientModuleDeclaration |>> AmbientModuleDeclaration

        str_ws "declare" >>. str_ws "module" >>. stringLiteral
        .>>. between (str_ws "{") (str_ws "}") (many ambientExternalModuleElement)
        |>> AmbientExternalModuleDeclaration
    ]


let rootReference =
    choice_attempt [
        str_ws "///" >>. str_ws "<reference" >>. str_ws "path=" >>. stringLiteral 
        .>> str_ws "/>" |>> File

        str_ws "///" >>. str_ws "<reference" >>. str_ws "no-default-lib=\"" >>. pbool_ws 
        .>> str_ws "\"" .>> str_ws "/>" |>> NoDefaultLib
    ]

let declarationElement =
    choice_attempt [
        exportAssignment .>> str_ws ";" |>> RootExportAssignment
        exportedness .>>. interfaceDeclaration |>> RootInterfaceDeclaration
        exportedness .>>. importDeclaration |>> RootImportDeclaration
        exportedness .>>. externalImportDeclaration |>> RootExternalImportDeclaration
        exportedness .>>. ambientDeclaration |>> RootAmbientDeclaration
        rootReference |>> RootReference
    ]

let declarationsFile : Parser<_, unit> = 
    ws >>. many declarationElement .>> eof
    |>> DeclarationsFile

let parseDeclarationsFile str = 
    match run declarationsFile str with
    | Success(r,_,_) -> r
    | Failure(msg,err,_) -> failwithf "%s" msg

//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\lib.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\jquery.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\jqueryui.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\knockout.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\fullCalendar.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\foundation.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\underscore.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\SharePoint.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\signalr.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\winjs.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\winrt.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\d3.d.ts")
//let lib = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\..\Examples\Typings\ember.d.ts")
//do  test declarationsFile lib

let tryOut() =
    test declarationsFile """
// Type definitions for When 2.4.0
// Project: https://github.com/cujojs/when
// Definitions by: Derek Cicerone <https://github.com/derekcicerone>
// Definitions: https://github.com/borisyankov/DefinitelyTyped

declare module When {
    function all<T>(promisesOrValues: any[]): Promise<T>;
}

export = When;

    """