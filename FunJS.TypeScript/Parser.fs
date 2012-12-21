module internal FunJS.TypeScript.Parser

open AST
open System.Reflection
open System.IO
open System.Text

let private withoutComments(stream:StreamReader) =
   let consume() = stream.Read() |> char
   let peek() = stream.Peek() |> char
   let throwAway() = consume() |> ignore
   let sb = StringBuilder()
   let append(c:char) =
      sb.Append c |> ignore

   let rec read() =
      if not stream.EndOfStream then
         match consume(), peek() with
         | '/', '/' -> throwAway(); ignoreLine()
         | '/', '*' -> throwAway(); ignoreCommentedSection()
         | c, _ -> 
            append c
            read()

   and ignoreLine() =
      if not stream.EndOfStream then
         printf "*"
         match consume() with
         | '\n' -> read()
         | _ -> ignoreLine()

   and ignoreCommentedSection() =
      if not stream.EndOfStream then
         match consume(), peek() with
         | '*', '/' -> throwAway(); read()
         | _ -> ignoreCommentedSection()

   read()
   sb.ToString()

let parse(stream:StreamReader) =
   let text = withoutComments stream

   // ranges
   let ws = set ['\n'; ' '; '\t'; '\r']
   let numeric = set ['0' .. '9']
   let lowercase = set ['a' .. 'z']
   let uppercase = set ['A' .. 'Z']
   let specialSymbols = set ['$'; '_']
   let alpha = lowercase + uppercase
   let alphaNumeric = alpha + numeric + specialSymbols
   let dot = set ['.']
   let typePath = alphaNumeric + dot

   let furthestPos = ref 0

   let skip (range:char Set) pos =
      let rec skip i =
         if i < text.Length && 
            range.Contains text.[i] 
         then skip (i+1)
         else i
      skip pos

   let startsWith (textToFind:string) pos =
      let pos = skip ws pos
      let rec startsWith i =
         i >= textToFind.Length || (
            (pos + i) < text.Length &&
            text.[pos + i] = textToFind.[i] &&
            startsWith (i+1))
      furthestPos := max pos !furthestPos
      if startsWith 0 then Some <| pos + textToFind.Length
      else None

   let (|Consume|_|) textToFind pos = startsWith textToFind pos

   let take (range:char Set) pos =
      let rec count i =
         if i < text.Length && range.Contains text.[i] then count (i+1)
         else i
      let pos = skip ws pos
      furthestPos := max pos !furthestPos
      match count pos with
      | n when n > pos -> 
         let toEmit = text.Substring(pos, n - pos)
         Some(toEmit, n)
      | _ -> None

   let (|Take|_|) = take

   let rec (|ListOf|_|) ((|Element|_|) as el) separator ending = function
      | Element (el, Consume separator (ListOf el separator ending (els, p))) -> Some(el::els, p)
      | Element (el, Consume ending p) -> Some([el], p)
      | Consume ending p -> Some([], p)
      | _ -> None
   
   let listOf el sep start ending = function
      | Consume start (ListOf el sep ending (els, p)) -> Some (els, p)
      | _ -> None

   let getTypeFromName = function
      | "void" -> Void
      | "bool" -> Boolean
      | "number" -> Number
      | "string" -> String
      | "any" -> Any
      | name -> Interface name

   let rec (|Type|_|) pos =
      let x = 10
      match pos with
      | Take alphaNumeric (name, Consume "[]" (Consume "[]" (Consume "[]" p))) ->
         Some(Array(Array(Array(getTypeFromName name))), p)
      | Take alphaNumeric (name, Consume "[]" (Consume "[]" p)) ->
         Some(Array(Array(getTypeFromName name)), p)
      | Take alphaNumeric (name, Consume "[]" p) ->
         Some(Array(getTypeFromName name), p)
      | Take alphaNumeric (name, p) ->
         let t = getTypeFromName name
         Some(t, p)
      | ParameterList (paras, Consume "=>" (Type(t, p))) ->
         Some(Lambda(paras, t), p)
      | ObjectPropertyList false (props, p) ->
         Some(Structural(props), p)
      | _ -> None

   and (|Var|_|) = function
      | Take alphaNumeric (name, Consume "?" (Consume ":" (Type (t, p)))) ->
         let var = 
           { TSVariable.Name = name
             TSVariable.Type = t
             TSVariable.IsOptional = true }
         Some(var, p)
      | Take alphaNumeric (name, Consume ":" (Type (t, p))) ->
         let var = 
           { TSVariable.Name = name
             TSVariable.Type = t
             TSVariable.IsOptional = false }
         Some(var, p)
      | Take alphaNumeric (name, Consume "?" p) ->
         let var = 
           { TSVariable.Name = name
             TSVariable.Type = Any
             TSVariable.IsOptional = true }
         Some(var, p)
      | Take alphaNumeric (name, p) ->
         let var = 
           { TSVariable.Name = name
             TSVariable.Type = Any
             TSVariable.IsOptional = false }
         Some(var, p)
      | _ -> None

   and (|Indexer|_|) p =
      match p with
      | Consume "[" (Take alphaNumeric (name, Consume ":" (Type (t, Consume "]" (Consume ":" (Type(retT, p))))))) ->
         let parameter =
           { TSParameter.Var = 
               {  TSVariable.Name = name
                  TSVariable.Type = t
                  TSVariable.IsOptional = false } 
             TSParameter.IsParamArray = false }
         let func =
           { TSFunction.Name = name
             TSFunction.IsOptional = false
             TSFunction.Parameters = [parameter]
             TSFunction.Type = retT }
         Some(func, p)
      | Consume "[" (Take alphaNumeric (name, Consume "]" (Type(retT, p)))) ->
         let parameter =
           { TSParameter.Var = 
               {  TSVariable.Name = name
                  TSVariable.Type = Any
                  TSVariable.IsOptional = false } 
             TSParameter.IsParamArray = false }
         let func =
           { TSFunction.Name = name
             TSFunction.IsOptional = false
             TSFunction.Parameters = [parameter]
             TSFunction.Type = retT }
         Some(func, p)
      | Consume "[" (Take alphaNumeric (name, Consume ":" (Type (t, Consume "]" p)))) ->
         let parameter =
           { TSParameter.Var = 
               {  TSVariable.Name = name
                  TSVariable.Type = t
                  TSVariable.IsOptional = false } 
             TSParameter.IsParamArray = false }
         let func =
           { TSFunction.Name = name
             TSFunction.IsOptional = false
             TSFunction.Parameters = [parameter]
             TSFunction.Type = Any }
         Some(func, p)
      | Consume "[" (Take alphaNumeric (name, Consume "]" p)) ->
         let parameter =
           { TSParameter.Var = 
               {  TSVariable.Name = name
                  TSVariable.Type = Any
                  TSVariable.IsOptional = false } 
             TSParameter.IsParamArray = false }
         let func =
           { TSFunction.Name = name
             TSFunction.IsOptional = false
             TSFunction.Parameters = [parameter]
             TSFunction.Type = Any }
         Some(func, p)
      | _ -> None

   and parameter = function
      | Consume "..." (Var (v, p)) ->
         let parameter =
            { TSParameter.Var = v
              TSParameter.IsParamArray = true }
         Some (parameter, p)
      | Var (v, p) ->
         let parameter =
            { TSParameter.Var = v
              TSParameter.IsParamArray = false }
         Some (parameter, p)
      | _ -> None
      
   and (|ParameterList|_|) = listOf parameter "," "(" ")"

   and (|Function|_|) = function
      | ParameterList (paras, Consume ":" (Type (t, p))) ->
         let func =
           { TSFunction.Name = ""
             TSFunction.Parameters = paras
             TSFunction.Type = t
             TSFunction.IsOptional = false }
         Some(func, p)
      | Take alphaNumeric (name, ParameterList (paras, Consume ":" (Type (t, p)))) ->
         let func =
           { TSFunction.Name = name
             TSFunction.Parameters = paras
             TSFunction.Type = t 
             TSFunction.IsOptional = false}
         Some(func, p)
      | Consume "?" (ParameterList (paras, Consume ":" (Type (t, p)))) ->
         let func =
           { TSFunction.Name = ""
             TSFunction.Parameters = paras
             TSFunction.Type = t
             TSFunction.IsOptional = true}
         Some(func, p)
      | Take alphaNumeric (name, Consume "?" (ParameterList (paras, Consume ":" (Type (t, p))))) ->
         let func =
           { TSFunction.Name = name
             TSFunction.Parameters = paras
             TSFunction.Type = t 
             TSFunction.IsOptional = true }
         Some(func, p)
      | ParameterList (paras, p) ->
         let func =
           { TSFunction.Name = ""
             TSFunction.Parameters = paras
             TSFunction.Type = Void
             TSFunction.IsOptional = false }
         Some(func, p)
      | Take alphaNumeric (name, ParameterList (paras, p)) ->
         let func =
           { TSFunction.Name = name
             TSFunction.Parameters = paras
             TSFunction.Type = Void 
             TSFunction.IsOptional = false }
         Some(func, p)
      | Consume "?" (ParameterList (paras, p)) ->
         let func =
           { TSFunction.Name = ""
             TSFunction.Parameters = paras
             TSFunction.Type = Void
             TSFunction.IsOptional = true }
         Some(func, p)
      | Take alphaNumeric (name, Consume "?" (ParameterList (paras, p))) ->
         let func =
           { TSFunction.Name = name
             TSFunction.Parameters = paras
             TSFunction.Type = Void 
             TSFunction.IsOptional = true }
         Some(func, p)
      | _ -> None

   and (|ObjProperty|_|) isStatic = function
      | Consume "static" (ObjProperty true (f, p)) ->
        Some(f, p)
      | Function (f, p) -> 
         let x = 0
         Some(Method(f,isStatic), p)
      | Indexer (f, p) -> Some(Indexer(f, isStatic), p)
      | Var (v, p) -> Some(Property(v, isStatic), p)      
      | _ -> None

   and objProperty = (|ObjProperty|_|)

   and (|ObjectPropertyList|_|) isStatic = listOf (objProperty isStatic) ";" "{" "}"

   let (|GlobalVar|_|) = function
      | Consume "var" (Var (v, Consume ";" p)) -> 
         Some(DeclareVar v, p)
      | _ -> None
   
   let (|GlobalFunc|_|) = function
      | Consume "function" (Function (f, Consume ";" p)) ->
         Some(DeclareFunction f, p)
      | _ -> None

   let (|GlobalObject|_|) = function
      | Consume "var" (Take alphaNumeric (name, Consume ":" (ObjectPropertyList true (props, p)))) ->
         let obj =
           { TSObject.Name = name
             TSObject.Members = props 
             TSObject.SuperTypes = [] }
         Some (DeclareObject obj, p)
      | _ -> None
   
   let (|SuperTypeList|_|) = listOf (take alphaNumeric) "," "extends" ""

   let (|GlobalInterface|_|) = function
      | Take alphaNumeric (name, SuperTypeList (superTypes, ObjectPropertyList false (props, p))) ->
         let obj =
           { TSObject.Name = name
             TSObject.Members = props 
             TSObject.SuperTypes = superTypes }
         Some(DeclareInterface obj, p)
      | Take alphaNumeric (name, ObjectPropertyList false (props, p)) ->
         let obj =
           { TSObject.Name = name
             TSObject.Members = props 
             TSObject.SuperTypes = [] }
         Some(DeclareInterface obj, p)
      | _ -> None

   let (|EnumCaseList|_|) = listOf (take alphaNumeric) "," "{" "}"

   let (|GlobalEnum|_|) = function
      | Consume "enum" (Take alphaNumeric (name, EnumCaseList (names, p))) ->
         Some(DeclareEnum(name, names), p)
      | _ -> None

   let rec (|GlobalModule|_|) = function
      | Consume "module" (Take typePath (name, DeclarationList (declarations, p))) ->
         Some(DeclareModule(name, declarations), p)
      | _ -> None

   and declaration = function
      | Consume "declare" p ->
         match p with
         | GlobalVar (v, p)
         | GlobalFunc (v, p)
         | GlobalEnum (v, p)
         | GlobalObject (v, p)
         | Consume "interface" (GlobalInterface (v, p))
         | GlobalModule (v, p) 
            -> Some(v, p)
         | _ -> None
      | Consume "export" p ->
         match p with
         | Consume "class" (GlobalInterface (DeclareInterface obj,p)) ->
            Some(DeclareClass obj, p)
         | GlobalVar (v, p)
         | GlobalFunc (v, p)
         | GlobalEnum (v, p)
         | Consume "interface" (GlobalInterface (v, p))
         | GlobalModule (v, p)
            -> Some(v, p)
         | _ -> None
      | GlobalModule (v,p)
      | GlobalEnum (v, p)
      | Consume "interface" (GlobalInterface (v, p))
         -> Some(v, p)
      | _ -> None

   and (|DeclarationList|_|) = listOf declaration "" "{" "}"

   let (|Declaration|_|) = declaration

   let rec rest acc = function
      | Declaration (d, p) -> rest (d::acc) p
      | p -> acc, p

   let results, p = rest [] 0
   let leftOver = text.Substring(!furthestPos).Trim()
   let hasLeftOvers = leftOver.Length > 0
   if hasLeftOvers then failwithf "error near: %s" leftOver
   else results
