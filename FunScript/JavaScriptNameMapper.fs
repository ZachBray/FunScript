module (*private*) FunScript.JavaScriptNameMapper

open System.Reflection

let keywords =
   set [
      "break"
      "case"
      "catch"
      "continue"
      "default"
      "delete"
      "do"
      "else"
      "finally"
      "for"
      "function"
      "if"
      "in"
      "instanceof"
      "new"
      "return"
      "switch"
      // TODO: It is hard to protect against "this" being used incorrectly
      //"this"
      "throw"
      "try"
      "typeof"
      "var"
      "void"
      "while"
      "with"
   ]


let reservedWords =
   set [
      "abstract"
      "boolean"
      "byte"
      "char"
      "class"
      "const"
      "debugger"
      "double"
      "enum"
      "export"
      "extends"
      "final"
      "float"
      "goto"
      "int"
      "interface"
      "implements"
      "import"
      "long"
      "native"
      "package"
      "private"
      "protected"
      "public"
      "short"
      "static"
      "super"
      "synchronized"
      "throws"
      "transient"
      "volatile"
   ]

let unsafeWords = keywords + reservedWords

let filterUnsafe str =
   if unsafeWords.Contains str then
      "_" + str
   else str

let sanitizeAux(str:string) =
   str |> Seq.map (function
      | c when (c >= 'a' && c <= 'z') || 
               (c >= '0' && c <= '9') ||
               (c >= 'A' && c <= 'Z') ||
               c = '$' ||
               c = '_' -> c
      | _ -> '_')
   |> Seq.toArray
   |> fun chars -> System.String(chars)
   |> filterUnsafe

let replacements = ref Map.empty
let used = ref Set.empty

let sanitize key str =
   let replacement = lazy sanitizeAux str
   // This is for when multiple unsafe strings map onto the
   // same safe string.
   let rec sanitize n =
      match !replacements |> Map.tryFind key with
      | Some replacement -> replacement
      | None ->
         let replacement =
            match n with
            | 0 -> replacement.Value
            | n -> sprintf "%s%i" replacement.Value n
         if !used |> Set.contains replacement then
            sanitize (n+1)
         else
            used := !used |> Set.add replacement
            replacements := !replacements |> Map.add key replacement
            replacement
   sanitize 0

let rec private getBestTypeName (t : System.Type) =
   let args =
      if t.IsGenericType || t.IsGenericTypeDefinition then
         t.GetGenericArguments()
      else [||]
   t.Name + "$" + (args |> Seq.map getBestTypeName |> String.concat "_")

let (*internal*) mapType (t : System.Type) =
   sanitize t.FullName (getBestTypeName t)

let getBestMethodName (mb : MethodBase) =
   let args =
      if mb.IsGenericMethod || mb.IsGenericMethodDefinition then
         mb.GetGenericArguments()
      else [||]
   (mapType mb.DeclaringType) + "$M_" + mb.Name + "$" + (args |> Seq.map mapType |> String.concat "_")

let (*internal*) mapMethod mb =
   let suggestedName = getBestMethodName mb
   let key =
      mb.DeclaringType.FullName + suggestedName
   sanitize key suggestedName