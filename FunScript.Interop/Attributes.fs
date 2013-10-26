namespace FunScript

open System

type JS = ReflectedDefinitionAttribute

[<AttributeUsage(AttributeTargets.Class||| AttributeTargets.Module ||| AttributeTargets.Field|||AttributeTargets.Property|||AttributeTargets.Method)>]
type InlineAttribute() =
   inherit System.Attribute()

[<AttributeUsage(AttributeTargets.Property|||AttributeTargets.Method)>]
type JSEmitAttribute(emit:string) =
   inherit System.Attribute()
   member __.Emit = emit

[<AttributeUsage(AttributeTargets.Property|||AttributeTargets.Method)>]
type JSEmitInlineAttribute(emit:string) =
   inherit System.Attribute()
   member __.Emit = emit

namespace global

open FunScript

[<AutoOpen>]
module TypeExtensions =
    
    [<JSEmitInlineAttribute("({})")>]
    let createEmpty<'a>() : 'a = failwith "never"