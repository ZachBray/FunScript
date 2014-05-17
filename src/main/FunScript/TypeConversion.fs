namespace FunScript

open AST
open Microsoft.FSharp.Quotations
open System

[<JS; AutoOpen>]
module internal Replacements =

    [<JSEmit("return parseInt({0});")>]
    let parseInt (x : string) : int = failwith "never"
    
    [<JSEmit("return parseFloat({0});")>]
    let parseFloat (x : string) : float = failwith "never"

    [<JSEmit("return isNaN({0});")>]
    let isNanInt (x : int) : bool = failwith "never"

    [<JSEmit("return isNaN({0});")>]
    let isNanFloat (x : float) : bool = failwith "never"

    let tryParseInt x =
        let y = parseInt x
        if isNanInt y then None
        else Some y

    let tryParseFloat x =
        let y = parseFloat x
        if isNanFloat y then None
        else Some y

    let tryParseBool x =
        match x with
        | "true" | "TRUE" | "True" -> Some true
        | "false" | "FALSE" | "False" -> Some false
        | _ -> None

    let parseBool x =
        match tryParseBool x with
        | None -> failwith ("Not a valid boolean value: " + x)
        | Some y -> y

[<JS; AutoOpen>]
module JSConversionExtensions =

    // TODO: Nullable might make more sense for these _value types_...

    type Boolean with static member TryParseJS x = tryParseBool x

    type Byte with static member TryParseJS x = tryParseInt x |> Option.map byte
    type UInt16 with static member TryParseJS x = tryParseInt x |> Option.map uint16
    type UInt32 with static member TryParseJS x = tryParseInt x |> Option.map uint32
    type UInt64 with static member TryParseJS x = tryParseInt x |> Option.map uint64

    type SByte with static member TryParseJS x = tryParseInt x |> Option.map sbyte
    type Int16 with static member TryParseJS x = tryParseInt x |> Option.map int16
    type Int32 with static member TryParseJS x = tryParseInt x
    type Int64 with static member TryParseJS x = tryParseInt x |> Option.map int64

    type Single with static member TryParseJS x = tryParseFloat x |> Option.map float32
    type Double with static member TryParseJS x = tryParseFloat x


module internal TypeConversions = 
    let components = 
        [
            ExpressionReplacer.create <@ Boolean.Parse @> <@ parseBool @>

            ExpressionReplacer.createUnsafe <@ Byte.Parse @> <@ parseInt @>
            ExpressionReplacer.createUnsafe <@ UInt16.Parse @> <@ parseInt @>
            ExpressionReplacer.createUnsafe <@ UInt32.Parse @> <@ parseInt @>
            ExpressionReplacer.createUnsafe <@ UInt64.Parse @> <@ parseInt @>

            ExpressionReplacer.createUnsafe <@ SByte.Parse @> <@ parseInt @>
            ExpressionReplacer.createUnsafe <@ Int16.Parse @> <@ parseInt @>
            ExpressionReplacer.create <@ Int32.Parse @> <@ parseInt @>
            ExpressionReplacer.createUnsafe <@ Int64.Parse @> <@ parseInt @>

            ExpressionReplacer.createUnsafe <@ Single.Parse @> <@ parseFloat @>
            ExpressionReplacer.create <@ Double.Parse @> <@ parseFloat @>

            //TODO: Consider whether we should support this... We might need our own decimal type. 
            ExpressionReplacer.createUnsafe <@ Decimal.Parse @> <@ parseFloat @>
        ]