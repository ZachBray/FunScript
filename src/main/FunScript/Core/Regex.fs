[<FunScript.JS>]
module FunScript.Core.Regex

open FunScript
open System.Collections
open System.Text.RegularExpressions

type GroupCollection =
    [<JSEmitInline("{0}")>]
    static member getArray(x: GroupCollection): Group[] = failwith "never"

    member xs.Item with get(i) = GroupCollection.getArray(xs).[i]
    member xs.Count with get() = GroupCollection.getArray(xs).Length
    member xs.GetEnumerator(): IEnumerator = upcast new ResizeArray.Enumerator<_>(GroupCollection.getArray(xs))
    static member ToSeq (xs: GroupCollection) =
        let xs = GroupCollection.getArray(xs)
        Seq.unfold (fun i -> if i < xs.Length then Some(xs.[i], i+1) else None) 0

type MatchCollection =
    [<JSEmitInline("{0}")>]
    static member getArray(x: MatchCollection): Match[] = failwith "never"

    member xs.Item with get(i) = MatchCollection.getArray(xs).[i]
    member xs.Count with get() = MatchCollection.getArray(xs).Length
    member xs.GetEnumerator(): IEnumerator = upcast new ResizeArray.Enumerator<_>(MatchCollection.getArray(xs))
    static member ToSeq (xs: MatchCollection) =
        let xs = MatchCollection.getArray(xs)
        Seq.unfold (fun i -> if i < xs.Length then Some(xs.[i], i+1) else None) 0

type Capture =
    [<JSEmit("return {0}.index !== undefined ? {0}.index : 0")>]
    static member getIndex(x: Capture): int = failwith "never"      // TODO: Index won't work for Groups
    [<JSEmit("return Array.isArray({0}) ? ({0}[0]) : {0}")>]
    static member private getValue(x: Capture): string = failwith "never"
    [<JSEmit("return Array.isArray({0}) ? ({0}[0]).length : {0}.length")>]
    static member private getLength(x: Capture): string = failwith "never"

    member x.Index with get() = Capture.getIndex(x)
    member x.Value with get() = Capture.getValue(x)
    member x.Length with get() = Capture.getLength(x)

type Group =
    [<JSEmitInline("({0} !== null)")>]
    static member getSucess(x: Group): bool = failwith "never"

    member x.Success with get() = Group.getSucess(x)

type Match =
    [<JSEmitInline("{0}")>]
    static member getGroups(x: Match): GroupCollection = failwith "never"

    member x.Groups with get() = Match.getGroups(x)

[<JSEmitInline("(new RegExp({0}, 'g' + {1}))")>]
let private createUnsafe(pattern: string, options: string): Regex = failwith "never"

[<JSEmitInline("{0}.global")>]
let private isGlobal(r: Regex): bool = failwith "never"
[<JSEmitInline("{0}.ignoreCase")>]
let private isIgnoreCase(r: Regex): bool = failwith "never"
[<JSEmitInline("{0}.multiline")>]
let private isMultiline(r: Regex): bool = failwith "never"

// TODO: Throw exception if ECMAScript is not set? But this would make Regex creation more verbose...
let private translateOptions(options: RegexOptions) =
    let isIgnoreCase = (options &&& RegexOptions.IgnoreCase) = RegexOptions.IgnoreCase
    let isMultiline = (options &&& RegexOptions.Multiline) = RegexOptions.Multiline
    match isIgnoreCase, isMultiline with
    | true, true -> "im"
    | true, false -> "i"
    | false, true -> "m"
    | false, false -> ""

let Create(pattern: string) =
    createUnsafe(pattern, "")

let CreateWithOptions(pattern: string, options: RegexOptions) =
    let options' = translateOptions options
    createUnsafe(pattern, options')

let GetOptions(r: Regex) =
    let options = RegexOptions.ECMAScript
    let options = if isIgnoreCase(r) then options ||| RegexOptions.IgnoreCase else options
    let options = if isMultiline(r)  then options ||| RegexOptions.Multiline  else options
    options

// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
[<JSEmit(@"return {0}.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, '\\$&')")>]
let Escape(s: string): string = failwith "never"
[<JSEmit(@"return {0}.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, '$1')")>]
let Unescape(s: string): string = failwith "never"

[<JSEmit("{0}.lastIndex = {2}; return {0}.test({1})")>]
let IsMatchWithOffset(r: Regex, input: string, offset: int): bool = failwith "never"
let IsMatch(r: Regex, input: string) = IsMatchWithOffset(r, input, 0)

let IsMatchStatic(input: string, pattern: string) =
    let regex = Create(pattern) in IsMatch(regex, input)
let IsMatchStaticWithOptions(input: string, pattern: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options) in IsMatch(regex, input)

[<JSEmit("{0}.lastIndex = {2}; return {0}.exec({1})")>]
let MatchFirstWithOffset(r: Regex, input: string, offset: int): Match = failwith "never"
let MatchFirst(r: Regex, input: string) = MatchFirstWithOffset(r, input, 0)

let MatchFirstStatic(input: string, pattern: string) =
    let regex = Create(pattern) in MatchFirst(regex, input)
let MatchFirstStaticWithOptions(input: string, pattern: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options) in MatchFirst(regex, input)

// NOTE: If, by any chance, a non-global regex is passed, we must throw an exception
//       to prevent falling into an infinite loop
[<JSEmit("""if (!{0}.global) { throw "Non-global RegExp" }
    var m, matches = [];
    {0}.lastIndex = {2};
    while ((m = {0}.exec({1})) !== null) { matches.push(m) }
    return matches""")>]
let MatchesWithOffset(r: Regex, input: string, offset: int): MatchCollection = failwith "never"
let Matches(r: Regex, input: string) = MatchesWithOffset(r, input, 0)

let MatchesStatic(input: string, pattern: string) =
    let regex = Create(pattern) in Matches(regex, input)
let MatchesStaticWithOptions(input: string, pattern: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options) in Matches(regex, input)

[<JSEmitInline("{1}.substring({3}).split({0}, {2})")>]
let SplitWithLimitAndOffset(r: Regex, input: string, limit: int, offset: int): string[] = failwith "never"
let SplitWithLimit(r: Regex, input: string, limit: int) = SplitWithLimitAndOffset(r, input, limit, 0)
let Split(r: Regex, input: string) = SplitWithLimit(r, input, -1)   // NOTE: JS will accept and ignore -1 as 'limit' argument

let SplitStatic(input: string, pattern: string) =
    let regex = Create(pattern) in Split(regex, input)
let SplitStaticWithOptions(input: string, pattern: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options) in Split(regex, input)

[<JSEmitInline("{1}.replace({0}, {2})")>]
let Replace(r: Regex, input: string, replacement: string): string = failwith "never"
let ReplaceWithLimitAndOffset(r: Regex, input: string, replacement: string, limit: int, offset: int) =
    let sub1 = input.Substring(offset)
    let matches = Matches(r, sub1)
    let sub2 =
        if matches.Count > limit
        then let m = matches.[limit - 1] in sub1.Substring(0, m.Index + m.Length)
        else sub1
    input.Substring(0, offset) + Replace(r, sub2, replacement) + input.Substring(offset + sub2.Length)
let ReplaceWithLimit(r: Regex, input: string, replacement: string, limit: int) = ReplaceWithLimitAndOffset(r, input, replacement, limit, 0)

let ReplaceStatic(input: string, pattern: string, replacement: string) =
    let regex = Create(pattern) in Replace(regex, input, replacement)
let ReplaceStaticWithOptions(input: string, pattern: string, replacement: string, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options) in Replace(regex, input, replacement)

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
let ReplaceWithEvaluatorAndLimitAndOffset(r: Regex, input: string, evaluator: MatchEvaluator, limit: int, offset: int): string = failwith "never"
let ReplaceWithEvaluatorAndLimit(r: Regex, input: string, evaluator: MatchEvaluator, limit: int) =
    ReplaceWithEvaluatorAndLimitAndOffset(r, input, evaluator, limit, 0)
let ReplaceWithEvaluator(r: Regex, input: string, evaluator: MatchEvaluator) =
    ReplaceWithEvaluatorAndLimit(r, input, evaluator, -1) // NOTE: JS will accept and ignore -1 as 'limit' argument

let ReplaceStaticWithEvaluator(input: string, pattern: string, evaluator: MatchEvaluator) =
    let regex = Create(pattern) in ReplaceWithEvaluator(regex, input, evaluator)
let ReplaceStaticWithEvaluatorAndOptions(input: string, pattern: string, evaluator: MatchEvaluator, options: RegexOptions) =
    let regex = CreateWithOptions(pattern, options) in ReplaceWithEvaluator(regex, input, evaluator)


