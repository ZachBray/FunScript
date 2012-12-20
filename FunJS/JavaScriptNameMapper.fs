module (*private*) FunJS.JavaScriptNameMapper

open System.Reflection

let sanitize(str:string) =
   str.Replace('.', '_')
      .Replace('+', '_')
      .Replace('`', '_')
      .Replace('@', '_')
      .Replace(''', '_')

let (*internal*) mapType(t:System.Type) =
   sanitize t.Name

let (*internal*) mapMethod(mi:MethodBase) =
   let prefix = 
      if mi.IsStatic then ""
      else "i_"
   prefix + mapType mi.DeclaringType + "_" + sanitize mi.Name