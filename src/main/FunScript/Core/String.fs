[<FunScript.JS>]
module FunScript.Core.String

// TODO: This is a bit tricky, it will never return a string but a function with valueOf and toString
// methods set to return the string. It's working for practical purposes but, should we improve it?
[<FunScript.JSEmit("""function formatToString(rep) {
		{0} = {0}.replace(/(?:^|[^%])%[+\-* ]?\d*(.\d+)?(\w)/, function(match, precision, format) {
			if (format == "f" && rep.toFixed !== undefined) {
				if (precision !== undefined) {
					rep = rep.toFixed(precision.substring(1));
				}
			}
			else if ((format == "E" || format == "e") && rep.toExponential !== undefined) {
				var pr = precision !== undefined ? precision.substring(1) : "";
				rep = rep.toExponential(pr);
			}
			return (match[0] !== '%') ? match[0] + rep : rep;
		})
		return formatToString;
	}
	formatToString.valueOf = formatToString.toString = function() { return {0}; }
	return formatToString""")>]
let PrintFormatToString(s: string): obj = failwith "never"

[<FunScript.JSEmit("""return {0}.replace(/{(\d+)(,-?\d+)?(:[CDEFGNPXTMYdt]\d{0,})?}/g, function(match, number, alignment, format) {
			var rep = match;
			if ({1}[number] !== undefined) {
				var rep = {1}[number];
				if (format !== undefined && format.length > 1) {
					var letter = format.substring(1,2);
					if (letter == "F" && rep.toFixed !== undefined) {
						var len = format.length > 2 ? format.substring(2) : "2";
						rep = rep.toFixed(len);
					}
					if (letter == "P" && rep.toFixed !== undefined) {
						var len = format.length > 2 ? format.substring(2) : "2";
						rep = (rep * 100).toFixed(len) + " %";
					}
					if (letter == "E" && rep.toExponential !== undefined) {
						rep = rep.toExponential(format.substring(2));
					}		
					else if (letter == "D" && rep.toDateString !== undefined) {
						rep = rep.toDateString();
					}
					else if (letter == "T" && rep.toLocaleTimeString !== undefined) {
						rep = rep.toLocaleTimeString();
					}
					else if (letter == "d" && rep.toLocaleDateString !== undefined) {
						rep = rep.toLocaleDateString();
					}
					else if (letter == "t" && rep.toLocaleTimeString !== undefined) {
						rep = rep.toLocaleTimeString().replace(/:\d\d(?!:)/, '')
					}
				}
			}
			return rep;
	})""")>]
let Format(s: string, [<System.ParamArray>] args: obj[]): string = failwith "never"

[<FunScript.JSEmitInline("{0}.split({1})")>]
let private splitSingle(s:string, delimiter:string) : string[] = failwith "never"

[<FunScript.JSEmitInline("{0}.replace({1}, {2})")>]
let private replaceSingle(s:string, search:string, replace:string) : string = failwith "never"

[<FunScript.JSEmitInline("{0}.indexOf({1})")>]
let IndexOfWithoutOffset(s:string, search:string) : int = failwith "never"

[<FunScript.JSEmitInline("{0}.indexOf({1}, {2})")>]
let IndexOfWithOffset(s:string, search:string, offset:int) : int = failwith "never"

[<FunScript.JSEmitInline("{0}.lastIndexOf({1})")>]
let LastIndexOfWithoutOffset(s: string, search: string) : int = failwith "never"

[<FunScript.JSEmitInline("{0}.lastIndexOf({1}, {2})")>]
let LastIndexOfWithOffset(s: string, search: string, offset:int) : int = failwith "never"

[<FunScript.JSEmitInline("{0}.trim()")>]
let Trim(s: string): string = failwith "never"

let StartsWith(s, search) =
    IndexOfWithoutOffset(s, search) = 0

let EndsWith(s: string, search: string) =
    let offset = s.Length - search.Length
    let index = IndexOfWithOffset(s, search, offset)
    index <> -1 && index = offset

[<FunScript.JSEmitInline("{0}.toLowerCase()")>]
let ToLowerCase(s:string) : string = failwith "never"

[<FunScript.JSEmitInline("{0}.toUpperCase()")>]
let ToUpperCase(s:string) : string = failwith "never"

[<FunScript.JSEmitInline("({0}==null)||({0}==\"\")")>]
let IsNullOrEmpty(s:string) : bool = failwith "never"

[<FunScript.JSEmitInline("{0}.length")>]
let Length(s:string) : int = failwith "never"

[<FunScript.JSEmitInline("{0}.charAt({1})")>]
let CharAt(s:string, length:int) : char = failwith "never"

[<FunScript.JSEmitInline("{0}.substring({1}, {1} + {2})")>]
let SubstringWithLength(s:string, offset:int, length:int) : char = failwith "never"

[<FunScript.JSEmitInline("{0}.substring({1})")>]
let SubstringWithoutLength(s:string, offset:int) : char = failwith "never"

[<FunScript.JSEmitInline("String.fromCharCode({0})")>]
let FromCharCode x = char x

let SplitWithoutOptions(s:string, delimiters:string[]) : string[] = 
    delimiters |> Array.fold (fun inputs delimiter ->
        inputs |> Array.map (fun inp -> splitSingle(inp, delimiter))
                |> Array.concat) [| s |]
  
let SplitWithOptions(s, delimiters, opts) =
    let parts = SplitWithoutOptions(s, delimiters)
    match opts with
    | System.StringSplitOptions.RemoveEmptyEntries -> 
        parts |> Array.filter ((<>) "")
    | _ -> parts

[<FunScript.JSEmitInline("{1}.join({0})")>]
let Join(separator:string, s:string[]) : string = failwith "never"

let Replace(s:string, search:string, replace:string) : string = 
    let splits = splitSingle(s, search)
    Join(replace,splits)

[<FunScript.JSEmitInline("{0}")>]
let ToCharArray(str:string) : char[] = failwith "never"

// Re-implementation of functions from Microsoft.FSharp.Core.StringModule
module FSharpString = 
   [<FunScript.JSEmit("return {0}==null?\"\":{0};")>]
   let private emptyIfNull (str:string) : string = failwith "never"

   let Concat sep (strings : seq<string>) =  
      Join(sep, Array.ofSeq strings)

   let Iterate (f : (char -> unit)) (str:string) =
      let str = emptyIfNull str
      for i = 0 to str.Length - 1 do
          f str.[i] 

   let IterateIndexed f (str:string) =
      let str = emptyIfNull str
      for i = 0 to str.Length - 1 do
          f i str.[i] 

   let Map (f: char -> char) (str:string) =
      let str = emptyIfNull str
      str.ToCharArray() |> Array.Map (fun c -> (f c).ToString()) |> Concat ""

   let MapIndexed (f: int -> char -> char) (str:string) =
      let str = emptyIfNull str
      str.ToCharArray() |> Array.MapIndexed (fun i c -> (f i c).ToString()) |> Concat ""

   let Collect (f: char -> string) (str:string) =
      let str = emptyIfNull str
      str.ToCharArray() |> Array.Map f |> Concat ""

   let Initialize (count:int) (initializer: int-> string) =
      if count < 0 then invalidArg "count" "String length must be non-negative"
      Array.Initialize count initializer |> Concat ""

   let Replicate (count:int) (str:string) =
      if count < 0 then  invalidArg "count" "String length must be non-negative"
      let str = emptyIfNull str
      Initialize count (fun _ -> str)

   let ForAll f (str:string) =
      let str = emptyIfNull str
      let rec check i = (i >= str.Length) || (f str.[i] && check (i+1)) 
      check 0

   let Exists f (str:string) =
      let str = emptyIfNull str
      let rec check i = (i < str.Length) && (f str.[i] || check (i+1)) 
      check 0  

   let Length (str:string) =
      let str = emptyIfNull str
      str.Length


