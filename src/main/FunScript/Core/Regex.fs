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
    // TODO: Index won't work for Group. But fixing it will require creating new objects in JS. Is it worth it?
    [<JSEmitInline("{0}.index")>]
    let Index(m: Capture): int = failwith "never"

    [<JSEmit("return Array.isArray({0}) ? ({0}[0]).length : {0}.length")>]
    let Length(m: Capture): int = failwith "never"
    
    [<JSEmit("return Array.isArray({0}) ? ({0}[0]) : {0}")>]
    let Value(m: Capture): string = failwith "never"

    let ToString(m: Capture) =
        m.Value

module Group = 
    [<JSEmitInline("({0} === null ? false : true)")>]
    let Success(m: Match): int = failwith "never"

module Match =
    [<JSEmitInline("({0}.GetEnumerator !== undefined)")>]
    let private hasEnumerator(o: obj): bool = failwith "never"

    [<JSEmitInline("{0}")>]
    let private groupsUnsafe(m: Match): GroupCollection = failwith "never"

    let Groups(m: Match) =
        let gs = groupsUnsafe(m)
        if not (hasEnumerator gs)
        then do FunScript.Core.Enumerator.AddArrayEnumerator(gs)
        gs


// TODO: Throw exception if ECMAScript is not set? But this would make Regex creation more verbose...
let private translateOptions(options: RegexOptions) =
    let isIgnoreCase = (options &&& RegexOptions.IgnoreCase) = RegexOptions.IgnoreCase
    let isMultiline = (options &&& RegexOptions.Multiline) = RegexOptions.Multiline
    match isIgnoreCase, isMultiline with
    | true, true -> "im"
    | true, false -> "i"
    | false, true -> "m"
    | false, false -> ""

[<JSEmitInline("(new RegExp({0}, 'g' + {1}))")>]
let private createUnsafe(pattern: string, options: string): Regex = failwith "neve"

let Create(pattern: string) =
    createUnsafe(pattern, "")

let CreateWithOptions(pattern: string, options: RegexOptions) =
    let options' = translateOptions options
    createUnsafe(pattern, options')

[<JSEmitInline("{0}.global")>]
let private isGlobal(r: Regex): bool = failwith "never"

[<JSEmitInline("{0}.ignoreCase")>]
let private isIgnoreCase(r: Regex): bool = failwith "never"

[<JSEmitInline("{0}.multiline")>]
let private isMultiline(r: Regex): bool = failwith "never"

let GetOptions(r: Regex) =
    let options = RegexOptions.ECMAScript
    let options = if isIgnoreCase(r) then options ||| RegexOptions.IgnoreCase else options
    let options = if isMultiline(r)  then options ||| RegexOptions.Multiline  else options
    options

[<JSEmitInline("{0}.source")>]
let ToString(r: Regex): string = failwith "never"

// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
[<JSEmit(@"return {0}.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, '\\$&')")>]
let Escape(s: string): string = failwith "never"

[<JSEmitInline(@"{0}.replace(/(\\)?\\/g , '$1')")>]
let Unescape(s: string): string = failwith "never"

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

// NOTE: If, by any chance, a non-global regex is passed, we must throw an exception
//       to prevent falling in an infinite loop
[<JSEmit("""if (!{0}.global) {
        throw "Non-global RegExp"
    }
    var m, matches = [];
    {0}.lastIndex = {2};
    while ((m = {0}.exec({1})) !== null) {
        matches.push(m)
    }
    return matches""")>]
let private matchesWithOffsetUnsafe(regex: Regex, input: string, offset: int): MatchCollection = failwith "never"

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

[<JSEmitInline("{1}.substring({3}).split({0}, {2})")>]
let private splitWithLimitAndOffsetUnsafe(regex: Regex, input: string, limit: int, offset: int): string[] = failwith "never"

let SplitWithLimitAndOffset(regex: Regex, input: string, limit: int, offset: int) =
    let ms = splitWithLimitAndOffsetUnsafe(regex, input, limit, offset)
    FunScript.Core.Enumerator.AddArrayEnumerator(ms)
    ms

let SplitWithLimit(regex: Regex, input: string, limit: int) =
    SplitWithLimitAndOffset(regex, input, limit, 0)

let Split(regex: Regex, input: string) =
    SplitWithLimit(regex, input, -1)                    // NOTE: JS will accept and ignore -1 as 'limit' argument

let SplitStatic(input: string, pattern: string) =
    let regex = Create(pattern)
    Split(regex, input)

let SplitStaticWithOptions(input: string, pattern: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options)
    Split(regex, input)

[<JSEmitInline("{1}.replace({0}, {2})")>]
let Replace(regex: Regex, input: string, replacement: string): string = failwith "never"

let ReplaceWithLimitAndOffset(regex: Regex, input: string, replacement: string, limit: int, offset: int) =
    let sub1 = input.Substring(offset)
    let matches = regex.Matches(sub1)
    let sub2 =
        if matches.Count > limit
        then let m = matches.[limit - 1] in sub1.Substring(0, m.Index + m.Length)
        else sub1
    input.Substring(0, offset) + Replace(regex, sub2, replacement) + input.Substring(offset + sub2.Length)

let ReplaceWithLimit(regex: Regex, input: string, replacement: string, limit: int) =
    ReplaceWithLimitAndOffset(regex, input, replacement, limit, 0)

let ReplaceStatic(input: string, pattern: string, replacement: string) =
    let regex = Create(pattern)
    Replace(regex, input, replacement)

let ReplaceStaticWithOptions(input: string, pattern: string, replacement: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options)
    Replace(regex, input, replacement)

[<JSEmit("""var replacer = function () {
    	var res = arguments[0];
		if ({3} !== 0) {
			{3}--;
			var match = [];
    	    var len = arguments.length;
            for (var i = 0; i < len-2; i ++) {
				match.push(arguments[i]);
			}
			match.index = arguments[len-2];
			match.input = arguments[len-1];
			res = {2}(match);
		}
		return res;
	}
	return {1}.substring(0, {4}) + {1}.substring({4}).replace({0}, replacer)""")>]
let ReplaceWithEvaluatorAndLimitAndOffset(regex: Regex, input: string, evaluator: MatchEvaluator, limit: int, offset: int): string = failwith "never"

let ReplaceWithEvaluatorAndLimit(regex: Regex, input: string, evaluator: MatchEvaluator, limit: int) =
    ReplaceWithEvaluatorAndLimitAndOffset(regex, input, evaluator, limit, 0)

let ReplaceWithEvaluator(regex: Regex, input: string, evaluator: MatchEvaluator) =
    ReplaceWithEvaluatorAndLimit(regex, input, evaluator, -1)     // NOTE: JS will accept and ignore -1 as 'limit' argument

let ReplaceStaticWithEvaluator(input: string, pattern: string, evaluator: MatchEvaluator) =
    let regex = Create(pattern)
    ReplaceWithEvaluator(regex, input, evaluator)

let ReplaceStaticWithEvaluatorAndOptions(input: string, pattern: string, evaluator: MatchEvaluator, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options)
    ReplaceWithEvaluator(regex, input, evaluator)





