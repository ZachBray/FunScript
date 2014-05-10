#if INTERACTIVE
#else
module FunScript.TypeScript.AST
#endif

type Identifier = string
type IsOptional = bool
type IsStatic = bool
type IsExport = bool

type EntityName = 
    /// IdentifierPath: Identifier | IdentifierPath . Identifier
    /// EntityName: Identifier ModuleName . Identifier
    /// TypeName: Identifier ModuleName . Identifier
    /// TypeQueryExpression: Identifier | TypeQueryExpression . IdentifierName 
    | EntityName of Identifier list

type ExternalModuleReference =
    /// ExternalImportDeclaration: import Identifier = ExternalModuleReference ;
    /// ExternalModuleReference: require ( StringLiteral )
    | ExternalModuleReference of string

type TypeArgument = Type

and TypeReference =
    /// TypeReference: TypeName TypeArguments[opt]
    | TypeReference of EntityName * TypeArgument list

and InterfaceExtendsClause =
    /// extends ClassOrInterfaceTypeList
    /// ClassOrInterfaceTypeList: ClassOrInterfaceType | ClassOrInterfaceTypeList , ClassOrInterfaceType
    /// ClassOrInterfaceType: TypeReference
    | InterfaceExtendsClause of TypeReference list

and TypeParameter = 
    /// TypeParameters: < TypeParameterList >
    /// TypeParameterList: TypeParameter | TypeParameterList , TypeParameter
    /// TypeParameter: Identifier Constraint[opt]
    /// Constraint: extends TypeReference
    /// ***** NOTE: THE SPEC SEEMS TO BE INCORRECT HERE
    ///             THE '*.d.ts' FILES USE Type AND NOT TypeReference
    | TypeParameter of Identifier * Type option

/// PropertyName: IdentifierName | StringLiteral | NumericLiteral
and PropertyName =
    | NameIdentifier of Identifier
    | NameStringLiteral of string
    | NameNumericLiteral of int

/// PredefinedType: any | number | boolean | string | void
and PredefinedType = Any | Number | Boolean | String | Void

/// PublicOrPrivate: public | private
and PublicOrPrivate = Public | Private

/// RequiredParameterList: RequiredParameter | RequiredParameterList , RequiredParameter
/// RequiredParameter: 
/// | PublicOrPrivate[opt] Identifier TypeAnnotation[opt] 
/// | Identifier : StringLiteral
and RequiredParameter =
    | VariableParameter of PublicOrPrivate option * Identifier * TypeAnnotation option
    | StringEnumParameter of Identifier * string

and RestParameter =
    /// RestParameter: ... Identifier TypeAnnotation[opt]
    | RestParameter of Identifier * TypeAnnotation option

and LiteralValue =
    | BooleanValue of bool
    | NumberValue of float
    | StringValue of string
    | NullValue
    | UndefinedValue
    | UnknownValue of string

/// OptionalParameterList: OptionalParameter | OptionalParameterList , OptionalParameter
and OptionalParameter =
    /// | PublicOrPrivate[opt] Identifier ? TypeAnnotation[opt] 
    | OptionalParameter of PublicOrPrivate option * Identifier * TypeAnnotation option
    /// | PublicOrPrivate[opt] Identifier TypeAnnotation[opt] Initialiser
    | DefaultParameter of PublicOrPrivate option * Identifier * TypeAnnotation option * LiteralValue
    /// ***** NOTE: NOT DECLARED IN SPEC BUT USED AND ACCEPTED BY COMPILER
    /// | PublicOrPrivate[opt] Identifier ? : StringLiteral
    

and ParameterList =
    /// ParameterList: 
    /// | RequiredParameterList 
    /// | OptionalParameterList 
    /// | RestParameter 
    /// | RequiredParameterList , OptionalParameterList 
    /// | RequiredParameterList , RestParameter 
    /// | OptionalParameterList , RestParameter 
    /// | RequiredParameterList , OptionalParameterList , RestParameter
    | ParameterList of RequiredParameter list * OptionalParameter list * RestParameter option
    

/// TypeLiteral: ObjectType | ArrayType | FunctionType | ConstructorType
and TypeLiteral = 
    | LiteralObjectType of ObjectType
    /// ArrayType: Type [ ]
    | LiteralArrayType of Type
    /// FunctionType: TypeParameters[opt] ( ParameterList[opt] ) => Type
    | LiteralFunctionType of TypeParameter list * ParameterList * Type
    /// ConstructorType: new TypeParameters[opt] ( ParameterList[opt] ) => Type
    | LiteralConstructorType of TypeParameter list * ParameterList * Type

/// TypeQuery: typeof TypeQueryExpression 
/// TypeQueryExpression: Identifier | TypeQueryExpression . IdentifierName 
and TypeQuery =
    | TypeQuery of EntityName

/// Type: PredefinedType | TypeReference | TypeLiteral | TypeQuery
and Type =
    | Predefined of PredefinedType
    | Reference of TypeReference
    | Query of TypeQuery
    | Literal of TypeLiteral

and TypeAnnotation =
    /// TypeAnnotation: : Type
    | TypeAnnotation of Type

and PropertySignature =
    /// PropertySignature: PropertyName ?[opt] TypeAnnotation[opt]
    | PropertySignature of PropertyName * IsOptional * TypeAnnotation option

and CallSignature =
    /// CallSignature: TypeParameters[opt] ( ParameterList[opt] ) TypeAnnotation[opt]
    | CallSignature of TypeParameter list * ParameterList * TypeAnnotation option

and ConstructSignature =    
    /// ConstructSignature: new TypeParameters[opt] ( ParameterList[opt] ) TypeAnnotation[opt]
    | ConstructSignature of TypeParameter list * ParameterList * TypeAnnotation option

/// IndexSignature: 
/// | [ Identifier : string ] TypeAnnotation 
/// | [ Identifier : number ] TypeAnnotation
and IndexSignature =
    | IndexSignatureString of Identifier * TypeAnnotation
    | IndexSignatureNumber of Identifier * TypeAnnotation

and MethodSignature =
    /// MethodSignature: PropertyName ?[opt] CallSignature
    | MethodSignature of PropertyName * IsOptional * CallSignature

/// TypeMemberList: TypeMember | TypeMemberList ; TypeMember
/// TypeMember: PropertySignature | CallSignature | ConstructSignature | IndexSignature | MethodSignature
and TypeMember =
    | MemberPropertySignature of PropertySignature
    | MemberCallSignature of CallSignature
    | MemberConstructSignature of ConstructSignature
    | MemberIndexSignature of IndexSignature
    | MemberMethodSignature of MethodSignature

and ObjectType =
    /// { TypeBody[opt] }
    /// TypeBody: TypeMemberList ;[opt]
    | ObjectType of TypeMember list

type ClassExtendsClause = TypeReference option
type ClassImplementsClause = TypeReference list
type ClassHeritage =
    /// ClassHeritage: ClassExtendsClause[opt] ImplementsClause[opt]
    /// ClassExtendsClause: extends ClassType
    /// ClassType: TypeReference
    /// ImplementsClause: implements ClassOrInterfaceTypeList
    /// ClassOrInterfaceTypeList: ClassOrInterfaceType | ClassOrInterfaceTypeList , ClassOrInterfaceType
    | ClassHeritage of ClassExtendsClause * ClassImplementsClause

/// AmbientMemberDeclaration: 
/// | PublicOrPrivate[opt] static[opt] PropertyName TypeAnnotation[opt] ; 
/// | PublicOrPrivate[opt] static[opt] PropertyName CallSignature ;
/// ***** NOTE: THIS IS NOT CONTAINED IN THE SPEC BUT ALSO SEEMS TO BE VALID
///             WHERE static IS THE IDENTIFIER RATHER THAN A KEYWORD
/// | PublicOrPrivate[opt] static CallSignature ;
type AmbientMemberDeclaration =
    | AmbientPropertyDeclaration of PublicOrPrivate option * IsStatic * PropertyName * TypeAnnotation option
    | AmbientMethodDeclaration of PublicOrPrivate option * IsStatic * PropertyName * CallSignature

/// AmbientClassBody: AmbientClassBodyElements[opt]
/// AmbientClassBodyElements: AmbientClassBodyElement | AmbientClassBodyElements AmbientClassBodyElement
/// AmbientClassBodyElement: AmbientConstructorDeclaration | AmbientMemberDeclaration | IndexSignature
type AmbientClassBodyElement =
    /// AmbientConstructorDeclaration: constructor ( ParameterList[opt] ) ;
    | AmbientConstructorDeclaration of ParameterList
    /// AmbientMemberDeclaration
    | AmbientMemberDeclaration of AmbientMemberDeclaration
    /// IndexSignature
    | AmbientIndexSignature of IndexSignature

/// AmbientEnumBody: AmbientEnumMemberList ,[opt]
/// AmbientEnumMemberList: AmbientEnumMember | AmbientEnumMemberList , AmbientEnumMember
/// AmbientEnumMember: PropertyName | PropertyName = IntegerLiteral
type AmbientEnumMember =
    | NamedCase of PropertyName
    | NumberedCase of PropertyName * int
    
type CommonAmbientElement =
    /// AmbientVariableDeclaration: var Identifier TypeAnnotation[opt] ;
    | AmbientVariableDeclaration of Identifier * TypeAnnotation option
    /// AmbientFunctionDeclaration: function Identifier CallSignature ;
    | AmbientFunctionDeclaration of Identifier * CallSignature
    /// AmbientClassDeclaration: class Identifier TypeParameters[opt] ClassHeritage { AmbientClassBody }
    | AmbientClassDeclaration of Identifier * TypeParameter list * ClassHeritage * AmbientClassBodyElement list
    /// AmbientEnumDeclaration: enum Identifier { AmbientEnumBody[opt] }
    | AmbientEnumDeclaration of Identifier * AmbientEnumMember list

type ImportDeclaration =
    /// ImportDeclaration: import Identifier = EntityName ;
    | ImportDeclaration of Identifier * EntityName

type ExternalImportDeclaration =
    /// ExternalImportDeclaration: import Identifier = ExternalModuleReference ;
    | ExternalImportDeclaration of Identifier * ExternalModuleReference

type InterfaceDeclaration =
    /// interface Identifier TypeParameters[opt] InterfaceExtendsClause[opt] ObjectType
    | InterfaceDeclaration of Identifier * TypeParameter list * InterfaceExtendsClause * ObjectType

type ExportAssignment =
    /// export = Identifier ;
    | ExportAssignment of Identifier

/// AmbientModuleBody: AmbientModuleElements[opt]
/// AmbientModuleElements: AmbientModuleElement | AmbientModuleElements AmbientModuleElement
/// AmbientModuleElement: 
/// | export[opt] AmbientVariableDeclaration 
/// | export[opt] AmbientFunctionDeclaration 
/// | export[opt] AmbientClassDeclaration 
/// | export[opt] InterfaceDeclaration 
/// | export[opt] AmbientEnumDeclaration 
/// | export[opt] AmbientModuleDeclaration 
/// | export[opt] ImportDeclaration
type AmbientModuleElement =
    | AmbientModuleElement of IsExport * CommonAmbientElement
    | InterfaceDeclarationElement of IsExport * InterfaceDeclaration
    | AmbientModuleDeclarationElement of IsExport * EntityName * AmbientModuleElement list
    | ImportDeclarationElement of IsExport * ImportDeclaration

/// AmbientExternalModuleBody: AmbientExternalModuleElements[opt]
/// AmbientExternalModuleElements: AmbientExternalModuleElement | AmbientExternalModuleElements AmbientExternalModuleElement
/// AmbientExternalModuleElement: AmbientModuleElement | ExportAssignment | export[opt] ExternalImportDeclaration
type AmbientExternalModuleElement =
    | ExternalAmbientModuleElement of AmbientModuleElement
    | ExternalExportAssignment of ExportAssignment
    | ExternalAbientImportDeclaration of IsExport * ExternalImportDeclaration

/// AmbientDeclaration:
/// | declare AmbientVariableDeclaration
/// | declare AmbientFunctionDeclaration
/// | declare AmbientClassDeclaration
/// | declare AmbientEnumDeclaration
/// | declare AmbientModuleDeclaration
/// | declare AmbientExternalModuleDeclaration
type AmbientDeclaration =
    | CommonAmbientElementDeclaration of CommonAmbientElement
    /// AmbientModuleDeclaration: module IdentifierPath { AmbientModuleBody }
    | AmbientModuleDeclaration of EntityName * AmbientModuleElement list
    /// module StringLiteral { AmbientExternalModuleBody }
    | AmbientExternalModuleDeclaration of string * AmbientExternalModuleElement list

type RootReference =
    /// /// <reference path="../jquery/jquery.d.ts" />
    | File of string
    /// /// <reference no-default-lib="true|false" />
    | NoDefaultLib of bool

type DeclarationElement =
    /// export = Identifier ;
    | RootExportAssignment of ExportAssignment
    /// export[opt] InterfaceDeclaration 
    | RootInterfaceDeclaration of IsExport * InterfaceDeclaration
    /// export[opt] ImportDeclaration 
    | RootImportDeclaration of IsExport * ImportDeclaration
    /// export[opt] ExternalImportDeclaration 
    | RootExternalImportDeclaration of IsExport * ExternalImportDeclaration
    /// export[opt] AmbientDeclaration
    | RootAmbientDeclaration of IsExport * AmbientDeclaration
    /// /// <reference ... />
    | RootReference of RootReference

/// See TypeScript Specification Appendix A.9 
/// (http://typescript.codeplex.com/releases/view/105503)
type DeclarationsFile = DeclarationsFile of DeclarationElement list

