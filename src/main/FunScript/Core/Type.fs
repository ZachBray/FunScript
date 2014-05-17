[<FunScript.JS>]
module FunScript.Core.Type
open FunScript

[<JSEmit("return {0}.Tag;")>]
let getTag (x : obj) : int = failwith "never"

[<JSEmit("return {0}[{1}];")>]
let getProp (x : obj) (propName:string) : obj = failwith "never"

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
    | TupleType of Construct * Type[]
    | ArrayType of Type
    
and Type(name, fullName, typeArgs, kind) =
   member __.Name : string = name
   member __.FullName : string = fullName
   member __.GetGenericArguments() : Type[] = typeArgs
   member __.IsArray =
      match kind with
      | ArrayType _ -> true
      | _ -> false
   member __.GetElementType() =
      match kind with
      | ArrayType t -> t
      | _ -> failwith "Not an array type."
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
        | TupleType _ -> true
        | _ -> false

    static member IsRecord (t : Type, _ : obj) =
        match t.Kind with
        | RecordType _ -> true
        | _ -> false

    static member GetUnionCases (t : Type, _ : obj) =
        match t.Kind with
        | UnionType ucis -> ucis
        | _ -> failwith "Not a union type."

    static member GetTupleElements (t : Type) =
        match t.Kind with
        | TupleType(_, ts) -> ts
        | _ -> failwith "Not a tuple type."

    static member GetRecordFields (t : Type, _ : obj) =
        match t.Kind with
        | RecordType(_, pis) -> pis
        | _ -> failwith "Not a record type."


[<JSEmit("return {0}.apply({0}, {1});")>]
let invoke(methodName : string, args : obj[]) : obj =
   failwith "never"

let mkDelayed (t : Type) (flags : obj) f =
    fun arg -> f(t, arg, flags)

let mkDelayedNoFlags (t : Type) f =
    fun arg -> f(arg, t) 

type FSharpValue() =

    static member PreComputeUnionTagReader (t : Type, _ : obj) : obj -> int =
        getTag

    static member MakeUnion(uci : UnionCaseInfo, args : obj[], _ : obj) =
        uci.Construct args

    static member MakeTuple(args : obj[], t : Type) =
        match t.Kind with
        | TupleType(c, _) -> c args
        | _ -> failwith "Not a tuple type."

    static member MakeRecord(t : Type, args : obj[], _ : obj) =
        match t.Kind with
        | RecordType(cons, _) -> cons args
        | _ -> failwith "Not a record type."

    static member PreComputeRecordConstructor(t, flags : obj) =
        fun args -> FSharpValue.MakeRecord(t, args, flags)

    static member PreComputeTupleConstructor t =
        fun args -> FSharpValue.MakeTuple(args, t)

    static member PreComputeUnionConstructor(uci, flags : obj) =
        fun args -> FSharpValue.MakeUnion(uci, args, flags)

    static member PreComputeRecordFieldReader (pi : PropertyInfo) =
        fun (args : obj) -> pi.GetValue(args, [||])

    static member PreComputeUnionReader (uci : UnionCaseInfo, flags : obj) =
        fun (args : obj) -> 
            uci.GetFields() |> Array.map (fun pi ->
                pi.GetValue(args, [||]))

    static member PreComputeTupleReader t =
        fun (args : obj) -> 
            FSharpType.GetTupleElements t 
            |> Array.mapi (fun i _ ->
                getProp args ("Item" + i.ToString()))