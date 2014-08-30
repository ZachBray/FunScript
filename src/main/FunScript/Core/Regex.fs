[<FunScript.JS>]
module FunScript.Core.Regex

open FunScript
open System.Text.RegularExpressions

module Match =
    [<JSEmitInline("{0}")>]
    let Groups(m: Match): GroupCollection = failwith "never"
    
    [<JSEmitInline("{0}.index")>]
    let Index(m: Match): int = failwith "never"

    // NOTE: See Group below, if I use different JSEmit strings for properties with same name
    // in Match and Group (like Length or Value) apparently they conflict
    [<JSEmitInline("(Array.isArray({0}) ? ({0}[0]).length : {0}.length)")>]
    let Length(m: Match): int = failwith "never"
    
    [<JSEmitInline("({0} === null ? false : true)")>]
    let Success(m: Match): int = failwith "never"
    
    [<JSEmitInline("(Array.isArray({0}) ? {0}[0] : {0})")>]
    let Value(m: Match): string = failwith "never"

module MatchCollection =
    [<JSEmitInline("{0}.length")>]
    let Count(m: MatchCollection): int = failwith "never"

    [<JSEmitInline("{0}[{1}]")>]
    let Item(m: MatchCollection, index: int): Match = failwith "never"

    let GetEnumerator(m: MatchCollection) = (unbox<Match[]> m).GetEnumerator()

// TODO: Try to implement Index?
module Group =
    [<JSEmitInline("(Array.isArray({0}) ? ({0}[0]).length : {0}.length)")>]
    let Length(m: Group): int = failwith "never"

    [<JSEmitInline("(Array.isArray({0}) ? {0}[0] : {0})")>]
    let Value(m: Group): string = failwith "never"

module GroupCollection =
    [<JSEmitInline("{0}.length")>]
    let Count(m: GroupCollection): int = failwith "never"

    [<JSEmitInline("{0}[{1}]")>]
    let Item(m: GroupCollection, index: int): Group = failwith "never"

    let GetEnumerator(m: GroupCollection) = (unbox<Group[]> m).GetEnumerator()


// TODO: Throw exception if ECMAScript is not set? But this would make Regex creation more verbose...
let private translateOptions(options: RegexOptions) =

    let isIgnoreCase = (options &&& RegexOptions.IgnoreCase) = RegexOptions.IgnoreCase
    let isMultiline = (options &&& RegexOptions.Multiline) = RegexOptions.Multiline

    match isIgnoreCase, isMultiline with
    | true, true -> "im"
    | true, false -> "i"
    | false, true -> "m"
    | _ -> ""

[<JSEmitInline("(new RegExp({1},{2})).test({0})")>]
let private isMatch(input: string, pattern: string, options: string): bool = failwith "never"

let IsMatch(input: string, pattern: string) =
    isMatch(input, pattern, "")

let IsMatchWithOptions(input: string, pattern: string, options: RegexOptions) =
    let options' = translateOptions options
    isMatch(input, pattern, options')

[<JSEmitInline("{0}.match(new RegExp({1},{2}))")>]
let private matchFirst(input: string, pattern: string, options: string): Match = failwith "never"

let MatchFirst(input: string, pattern: string) =
    matchFirst(input, pattern, "")

let MatchFirstWithOptions(input: string, pattern: string, options: RegexOptions) =
    let options' = translateOptions options
    matchFirst(input, pattern, options')

[<JSEmit("var r = new RegExp({1},{2}); var ms = []; var m; while ((m = r.exec({0})) !== null) { ms.push(m) } return ms")>]
let private matches(input: string, pattern: string, options: string): MatchCollection = failwith "never"

let Matches(input: string, pattern: string) =
    matches(input, pattern, "g")

let MatchesWithOptions(input: string, pattern: string, options: RegexOptions) =
    let options' = "g" + translateOptions options
    matches(input, pattern, options')

[<JSEmitInline("{0}.split(new RegExp({1},{2}))")>]
let private split(input: string, pattern: string, options: string): string[] = failwith "never"

let Split(input: string, pattern: string) =
    split(input, pattern, "g")

let SplitWithOptions(input: string, pattern: string, options: RegexOptions) =
    let options' = "g" + translateOptions options
    split(input, pattern, options')

[<JSEmitInline("{0}.replace(new RegExp({1},{3}), {2})")>]
let private replace(input: string, pattern: string, replacement: string, options: string): string = failwith "never"

let Replace(input: string, pattern: string, replacement: string) =
    replace(input, pattern, replacement, "g")

let ReplaceWithOptions(input: string, pattern: string, replacement: string, options: RegexOptions) =
    let options' = "g" + translateOptions options
    replace(input, pattern, replacement, options')



