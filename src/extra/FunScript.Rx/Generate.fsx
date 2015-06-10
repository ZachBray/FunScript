#r "../../main/FunScript/bin/Debug/FunScript.dll"
#r "../../packages/FSharp.Control.Reactive/lib/net40/FSharp.Control.Reactive.dll"
#r "../../packages/Rx-Core/lib/net45/System.Reactive.Core.dll"
#r "../../packages/Rx-Linq/lib/net45/System.Reactive.Linq.dll"
#r "../../packages/Rx-Interfaces/lib/net45/System.Reactive.Interfaces.dll"
#load "Data.fsx"

open System
open System.Reflection
open System.IO
open System.Net
open FunScript
open FunScript.Rx.Data
open FSharp.Control.Reactive

let mappingsFile = __SOURCE_DIRECTORY__ + @"\Mappings.txt"

let observableType = 
    let mi, _ = Quote.toMethodInfoFromLambdas <@ Observable.aggregate @>
    mi.DeclaringType

//let unsupportedKeywords =
//    [|
//        "SynchronizationContext"
//        "Timestamped"
//        "Scheduler"
//        "Subject"
//        "Plan"
//        "Grouped"
//        "IConnectableObservable"
//        "Notification"
//        "TimeInterval"
//        "Task"
//    |]

let requiresImpl =
    [|
        "IEnumerable<", "array<", sprintf "(Seq.toArray %s)"
        "IObservable<IEnumerable<", "IObservable<array<", sprintf "(Observable.map Seq.toArray %s)" 
        "TimeSpan", "float", sprintf "%s.TotalMilliseconds"
        "DateTime", "float", sprintf "%s.TotalMilliseconds"
    |]

let funcName = typeof<_ -> _>.Name

let safeTypeName(name : string) =
    match name.IndexOf("`") with
    | -1 -> name
    | i -> name.Substring(0, i)

let rec printType(t : Type) =
    if t.IsArray && t.GetArrayRank() = 1 then 
        sprintf "%s[]" (printType (t.GetElementType()))
    elif t.IsGenericType || t.IsGenericTypeDefinition then
        let args = t.GetGenericArguments()
        if t.Name = funcName && args.Length = 2 then
            sprintf "%s -> %s" (printType args.[0]) (printType args.[1])
        else
            let typeArgs = 
                args
                |> Array.map printType
                |> String.concat ", "
            sprintf "%s<%s>" (safeTypeName t.Name) typeArgs
    elif t.IsGenericParameter then sprintf "'%s" t.Name
    else t.Name
//
//let containsUnsupportedKeyword t =
//    let name = printType t
//    unsupportedKeywords |> Array.exists (fun kw ->
//        name.Contains kw)

//let supportedMethods =
//    methods 
//    |> Seq.choose (fun mi ->
//        supportedInstanceMethods |> Seq.tryFind (fun m -> 
//            mi.Name.StartsWith m
//            && mi.GetParameters().Length >= 1 
//            && not (containsUnsupportedKeyword mi.ReturnType)
//            && not (mi.GetParameters() |> Seq.exists (fun p -> 
//                        containsUnsupportedKeyword p.ParameterType)))
//        |> Option.map (fun m -> m, mi))
//    |> Seq.sortBy (fun (_, mi) -> mi.Name)
//    |> Seq.toArray
//
let prelude = """[<FunScript.JS>]
module FunScript.Rx.Observable

open System
open System.Collections.Generic
open System.Reactive
open System.Reactive.Linq
open FunScript

"""

type MethodMapping =
    | Instance of string option
    | Static of string option

let mappings = 
    if File.Exists mappingsFile then 
        File.ReadAllLines(mappingsFile) 
        |> Seq.choose(fun line -> 
            match line.Split [|':'|] with
            | [|name; "i"|] -> Some(name, Instance None)
            | [|name; "s"|] -> Some(name, Static None)
            | [|name; "i"; methodOverride|] -> Some(name, Instance(Some methodOverride))
            | [|name; "s"; methodOverride|] -> Some(name, Static(Some methodOverride))
            | _ -> None)
        |> Map.ofSeq
    else Map.empty

let findRxJsMethodName(mi : MethodInfo) =
    let rxJsMethodName =
        supportedInstanceMethods |> Seq.tryFind (fun m -> 
            mi.Name.StartsWith m)
    match rxJsMethodName with
    | None -> mi.Name
    | Some m -> m

let tryCreateAttribute (writer : StringWriter) str methodName =
    let mi = observableType.GetMethod methodName
    let m = findRxJsMethodName mi
    let parameters = mi.GetParameters()
    let args = 
        match parameters.Length with
        | 0 -> "()"
        | n ->
            parameters.[0 .. parameters.Length - 2] 
            |> Array.mapi (fun i _ -> sprintf "{%i}" i)
            |> String.concat ", "


    let askForDetails() =
        printfn ">> %s" str
        //let link = sprintf "https://github.com/Reactive-Extensions/RxJS/blob/master/doc/api/core/operators/%s.md" m
        //Diagnostics.Process.Start(link) |> ignore
        printfn "(i)nstance or (s)tatic for %s?" mi.Name
        let input = System.Console.ReadLine().Trim()
        File.AppendAllLines(mappingsFile, [mi.Name + ":" + input])
        match input.Split [|':'|] with
        | [|"i"|] -> Some(Instance None)
        | [|"s"|] -> Some(Static None)
        | [|"i"; methodOverride|] -> Some(Instance(Some methodOverride))
        | [|"s"; methodOverride|] -> Some(Static(Some methodOverride))
        | _ -> None

    let details =
        match mappings.TryFind mi.Name with
        | None -> None // askForDetails()
        | x -> x
    
    match details with
    | None -> false
    | Some mapping ->
        let needsImpl = 
            parameters |> Array.map (fun p -> printType p.ParameterType)
            |> Array.exists (fun name ->
                requiresImpl |> Array.exists (fun (n, _, _) -> name.StartsWith n))
        
        let m =
            match mapping with
            | Instance None | Static None -> m
            | Instance(Some m') | Static(Some m') -> m'
        let ffi =
            match mapping with
            | Instance _ ->
                sprintf "[<JSEmitInline(\"{%i}.%s(%s)\")>]" (parameters.Length - 1) m args
            | Static _ -> 
                sprintf "[<JSEmitInline(\"Rx.Observable.%s({%i}, %s)\")>]" m (parameters.Length - 1) args
        writer.WriteLine ffi

        if needsImpl then
            let implParamDefs =
                parameters |> Array.map (fun p ->
                    let typeName = printType p.ParameterType 
                    let alternativeName =
                        requiresImpl |> Array.tryPick (fun (n, r, _) -> 
                            if typeName.StartsWith n then Some(r + typeName.Substring n.Length)
                            else None)
                    let typeName = defaultArg alternativeName typeName
                    sprintf "(%s : %s)" p.Name typeName)
                |> String.concat " "
            let paramDefs =
                parameters |> Array.map (fun p -> 
                    sprintf "(%s : %s)" p.Name (printType p.ParameterType))
                |> String.concat " "
            let args =
                parameters |> Array.map (fun p -> 
                    let typeName = printType p.ParameterType
                    let argAdjuster =
                        requiresImpl |> Array.tryPick (fun (n, _, f) -> 
                            if typeName.StartsWith n then Some f
                            else None)
                    let argAdjuster = defaultArg argAdjuster id
                    argAdjuster p.Name)
                |> String.concat " "
            let definition =
                sprintf "let %sImpl %s = failwith \"JavaScript only\"" methodName implParamDefs
            let call =
                sprintf "let %s %s = %sImpl %s" methodName paramDefs methodName args
            writer.WriteLine definition
            writer.WriteLine ""
            writer.WriteLine call
            writer.WriteLine ""
            false
        else
            true
        

let getSource() =
    async {
        let request = WebRequest.Create("https://raw.githubusercontent.com/fsprojects/FSharp.Control.Reactive/master/src/Observable.fs")
        let! response = request.AsyncGetResponse()
        use stream = response.GetResponseStream()
        use reader = new StreamReader(stream)
        return! Async.AwaitTask(reader.ReadToEndAsync())
    } |> Async.RunSynchronously

let createTemplate() =
    //if File.Exists mappingsFile then File.Delete mappingsFile
    let source = getSource()
    let lines = source.Split [|'\n'|]
    use reader = new StringReader(source)
    use writer = new StringWriter()
    writer.WriteLine prelude
    while not(reader.ReadLine().Contains "F# combinator wrappers for Rx extensions") do ()
    reader.ReadLine() |> ignore
    let letPrefix = "    let "
    let tryWriteAttribute(line : string) =
        let lineAfterLet = line.Substring(letPrefix.Length)
        let methodName = 
            match lineAfterLet.IndexOfAny [|' '; '<'; '('; '='; '\r'; '\n' |] with
            | -1 -> lineAfterLet
            | nextTokenIndex -> lineAfterLet.Substring(0, nextTokenIndex)
        tryCreateAttribute writer line methodName
    let rec writeMethods(isCommented) =
        match reader.ReadLine() with
        | null -> ()
        | str ->
            let isCommented =
                if str.StartsWith letPrefix then not(tryWriteAttribute str)
                else isCommented
            let isComment = str.TrimStart().StartsWith "//"
            if isCommented && not isComment then writer.Write "//"
            let hasPadding = str |> Seq.takeWhile ((=) ' ') |> Seq.length >= 4
            writer.WriteLine (if hasPadding then str.Substring 4 else str)
            writeMethods(isCommented)
    writeMethods(false)
    let contents = writer.ToString()
    let file = __SOURCE_DIRECTORY__ + @"\Observable.fs"
    printf "Writing %i chars to %s" contents.Length file
    File.WriteAllText(file, contents)

do  createTemplate()