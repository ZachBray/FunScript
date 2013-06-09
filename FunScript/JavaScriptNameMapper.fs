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
      str + "_reserved"
   else str

let sanitizeAux(str:string) =
   str |> Seq.map (function
      | c when (c >= 'a' && c <= 'z') || 
               (c >= '0' && c <= '9') ||
               (c >= 'A' && c <= 'Z') ||
               c = '_' -> c
      | _ -> '_')
   |> Seq.toArray
   |> fun chars -> System.String(chars)
   |> filterUnsafe

let replacements = ref Map.empty
let used = ref Set.empty

let sanitize str =
   let replacement = sanitizeAux str
   let rec sanitize n =
      match !replacements |> Map.tryFind str with
      | Some replacement -> replacement
      | None ->
         let replacement =
            match n with
            | 0 -> replacement
            | n -> sprintf "%s%i" replacement n
         if !used |> Set.contains replacement then
            sanitize (n+1)
         else
            used := !used |> Set.add replacement
            replacements := !replacements |> Map.add str replacement
            replacement
   sanitize 0

let (*internal*) mapType(t:System.Type) =
   sanitize t.Name

let (*internal*) mapMethod(mi:MethodBase) =
   let prefix = 
      if mi.IsStatic then ""
      else "i_"
   prefix + mapType mi.DeclaringType + "_" + sanitize mi.Name