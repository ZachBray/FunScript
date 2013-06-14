[<FunScript.JS>]
module FunScript.Core.Type

[<FunScript.JSEmit("return {0}.Tag;")>]
let getTag (x : obj) : string = failwith "never"

type PropertyInfo(name, f, getPropType) =
   member __.Name : string = name
   member __.GetValue (obj : obj, args : obj[]) : obj = f obj
   member __.PropertyType : Type = getPropType()

and UnionCaseInfo(name, fields) =  
   member __.Name : string = name
   member __.GetFields() : PropertyInfo [] = fields

and TypeKind =
    | ClassType
    | RecordType of PropertyInfo []
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
    static member IsTuple (t : Type) = t.Kind = TupleType
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
        | RecordType pis -> pis
        | _ -> failwith "Not a record type."


type FSharpValue() =
   static member PreComputeUnionTagReader (t : Type, _ : obj) =
      let ucis = FSharpType.GetUnionCases(t, None)
      fun (obj : obj) ->
         let tagName = getTag obj
         ucis |> Array.findIndex (fun uci -> uci.Name = tagName)