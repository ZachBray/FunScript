[<FunScript.JS>]
module FunScript.Core.Regex

open FunScript
open System.Text.RegularExpressions

module Collection =
    open System.Collections

    [<JSEmitInline("{0}.length")>]
    let Count (x: IEnumerable): int = failwith "never"

    [<JSEmitInline("{0}[{1}]")>]
    let Item (x: IEnumerable, index: int): Match = failwith "never"

    [<JSEmitInline("{0}.GetEnumerator()")>]
    let GetEnumerator (x: IEnumerable): IEnumerator = failwith "never"

module Capture =
    [<JSEmitInline("{0}.index")>]
    let Index(m: Capture): int = failwith "never"

    [<JSEmitInline("(Array.isArray({0}) ? ({0}[0]).length : {0}.length)")>]
    let Length(m: Capture): int = failwith "never"
    
    [<JSEmitInline("(Array.isArray({0}) ? ({0}[0]) : {0})")>]
    let Value(m: Capture): string = failwith "never"

module Group = 
    [<JSEmitInline("({0} === null ? false : true)")>]
    let Success(m: Match): int = failwith "never"

module Match =
    [<JSEmitInline("{0}")>]
    let private groupsUnsafe(m: Match) = failwith "never"

    let Groups(m: Match): GroupCollection =
        let gs = groupsUnsafe(m)
        FunScript.Core.Enumerator.AddArrayEnumerator(gs)
        gs

// TODO: Throw exception if ECMAScript is not set? But this would make Regex creation more verbose...
let private translateOptions(options: RegexOptions) =

    let isIgnoreCase = (options &&& RegexOptions.IgnoreCase) = RegexOptions.IgnoreCase
    let isMultiline = (options &&& RegexOptions.Multiline) = RegexOptions.Multiline

    match isIgnoreCase, isMultiline with
    | true, true -> "im"
    | true, false -> "i"
    | false, true -> "m"
    | _ -> ""

[<JSEmitInline("(new RegExp({0}, 'g' + {1}))")>]
let private createUnsafe(pattern: string, options: string): Regex = failwith "neve"

let Create(pattern: string) =
    createUnsafe(pattern, "")

let CreateWithOptions(pattern: string, options: RegexOptions) =
    let options' = translateOptions options
    createUnsafe(pattern, options')   

[<JSEmit("{0}.lastIndex = {2}; return {0}.test({1})")>]
let IsMatchWithOffset(regex: Regex, input: string, offset: int): bool = failwith "never"

let IsMatch(regex: Regex, input: string) =
    IsMatchWithOffset(regex, input, 0)

let IsMatchStatic(input: string, pattern: string) =
    let regex = Create(pattern)
    IsMatch(regex, input)

let IsMatchStaticWithOptions(input: string, pattern: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options)
    IsMatch(regex, input)

[<JSEmit("{0}.lastIndex = {2}; return {0}.exec({1})")>]
let MatchFirstWithOffset(regex: Regex, input: string, offset: int): Match = failwith "never"

let MatchFirst(regex: Regex, input: string) =
    MatchFirstWithOffset(regex, input, 0)

let MatchFirstStatic(input: string, pattern: string) =
    let regex = Create(pattern)
    MatchFirst(regex, input)

let MatchFirstStaticWithOptions(input: string, pattern: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options)
    MatchFirst(regex, input)

[<JSEmit("{0}.lastIndex = {2}; var matches = []; var m; while ((m = {0}.exec({1})) !== null) { matches.push(m) } return matches")>]
let matchesWithOffsetUnsafe(regex: Regex, input: string, offset: int): MatchCollection = failwith "never"

let MatchesWithOffset(regex: Regex, input: string, offset: int): MatchCollection =
    let ms = matchesWithOffsetUnsafe(regex, input, offset)
    FunScript.Core.Enumerator.AddArrayEnumerator(ms)
    ms

let Matches(regex: Regex, input: string) =
    MatchesWithOffset(regex, input, 0)

let MatchesStatic(input: string, pattern: string) =
    let regex = Create(pattern)
    Matches(regex, input)

let MatchesStaticWithOptions(input: string, pattern: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options)
    Matches(regex, input)

[<JSEmitInline("{1}.split({0})")>]
let Split(regex: Regex, input: string): string[] = failwith "never"

let SplitStatic(input: string, pattern: string) =
    let regex = Create(pattern)
    Split(regex, input)

let SplitStaticWithOptions(input: string, pattern: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options)
    Split(regex, input)

[<JSEmitInline("{1}.replace({0}, {2})")>]
let Replace(regex: Regex, input: string, replacement: string): string = failwith "never"

let ReplaceStatic(input: string, pattern: string, replacement: string) =
    let regex = Create(pattern)
    Replace(regex, input, replacement)

let ReplaceStaticWithOptions(input: string, pattern: string, replacement: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options)
    Replace(regex, input, replacement)



