module FunJS.TypeScript.AST

type name = string

type TSType =
   | Any
   | Number
   | Boolean
   | String
   | Void
   | GlobalObject of name
   | Enumeration of name
   | Class of name
   | Interface of name
   | Array of TSType
   | Lambda of TSParameter list * TSType
   | Structural of TSObjectMember list

and TSVariable =
 { Name: name
   Type: TSType
   IsOptional: bool }

and TSParameter = 
 { Var: TSVariable
   IsParamArray: bool }

and TSFunction =
 { Name: name
   Type: TSType
   Parameters: TSParameter list
   IsOptional: bool }

and isStatic = bool

and TSObjectMember =
   | Property of TSVariable * isStatic
   | Method of TSFunction * isStatic
   | Indexer of TSFunction * isStatic

type TSObject =
 { Name: string
   Members: TSObjectMember list
   SuperTypes: string list }

type TSGlobal =
   | DeclareVar of TSVariable
   | DeclareFunction of TSFunction
   | DeclareInterface of TSObject
   | DeclareClass of TSObject
   | DeclareObject of TSObject
   | DeclareModule of name * TSGlobal list
   | DeclareEnum of name * name list