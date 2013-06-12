[<FunScript.JS>]
module FunScript.Core.Type

type Type(name, fullName, typeArgs) =
   member __.Name : string = name
   member __.FullName : string = fullName
   member __.GetGenericArguments() : Type[] = typeArgs
//   member __.IsGenericType = isGenericType
//   member __.IsGenericTypeDefinition = isGenericTypeDefinition