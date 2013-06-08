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

let sanitize(str:string) =
   str.Replace('.', '_')
      .Replace('+', '_')
      .Replace('`', '_')
      .Replace('@', '_')
      .Replace(''', '_')
   |> filterUnsafe

let (*internal*) mapType(t:System.Type) =
   sanitize t.Name

let (*internal*) mapMethod(mi:MethodBase) =
   let prefix = 
      if mi.IsStatic then ""
      else "i_"
   prefix + mapType mi.DeclaringType + "_" + sanitize mi.Name