[<FunScript.JS>]
module FunScript.Core.Type
open FunScript

[<JSEmit("return {0}.Tag;")>]
let getTag (x : obj) : int = failwith "never"

type PropertyInfo(name, f, getPropType) =
   member __.Name : string = name
   member __.GetValue (obj : obj, args : obj[]) : obj = f obj
   member __.PropertyType : Type = getPropType()

and Construct = obj[] -> obj

and UnionCaseInfo(name, tag, cons, fields) =  
   member __.Name : string = name
   member __.Tag = tag
   member __.Construct (args : obj[]) : obj = cons args
   member __.GetFields() : PropertyInfo [] = fields

and TypeKind =
    | ClassType
    | RecordType of Construct * PropertyInfo []
    | UnionType of UnionCaseInfo []
    | TupleType
    
and Type(name, fullName, typeArgs, kind) =
   member __.Name : string = name
   member __.FullName : string = fullName
   member __.GetGenericArguments() : Type[] = typeArgs
   member __.Kind : TypeKind = kind
//   member __.IsGenericType = isGenericType
//   member __.IsGenericTypeDefinition = isGenericTypeDefinition
         

type FSharpType() =

    static member IsUnion (t : Type, _ : obj) = 
        match t.Kind with
        | UnionType _ -> true
        | _ -> false

    static member IsTuple (t : Type) = 
        match t.Kind with
        | TupleType -> true
        | _ -> false

    static member IsRecord (t : Type, _ : obj) =
        match t.Kind with
        | RecordType _ -> true
        | _ -> false

    static member GetUnionCases (t : Type, _ : obj) =
        match t.Kind with
        | UnionType ucis -> ucis
        | _ -> failwith "Not a union type."

    static member GetRecordFields (t : Type, _ : obj) =
        match t.Kind with
        | RecordType(_, pis) -> pis
        | _ -> failwith "Not a record type."


[<JSEmit("return {0}.apply({0}, {1});")>]
let invoke(methodName : string, args : obj[]) : obj =
   failwith "never"

type FSharpValue() =

   static member PreComputeUnionTagReader (t : Type, _ : obj) : obj =
      // TODO: Fix returning lambda bug!
      box getTag

   static member MakeUnion(uci : UnionCaseInfo, args : obj[], _ : obj) =
      uci.Construct args

   static member MakeRecord(t : Type, args : obj[], _ : obj) =
      match t.Kind with
      | RecordType(cons, _) -> cons args
      | _ -> failwith "Not a record type."