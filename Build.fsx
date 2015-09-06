#r @"packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.AssemblyInfoFile
open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let mainBuildDir    = "build/main/bin/"
let mainPackageDir  = "build/main/deploy/"

let dataBuildDir    = "build/data/bin/"
let dataPackageDir  = "build/data/deploy/"

let typescriptBuildDir = "build/typescript/bin/"
let typescriptPackageDir = "build/typescript/deploy/"

let rxBuildDir      = "build/rx/bin/"
let rxPackageDir    = "build/rx/deploy/"

let testBuildDir    = "build/tests/"

let dependenciesDir = "src/packages/"
let versionNumber =
    match buildServer with
    | TeamCity -> buildVersion
    | _ -> "0.0.0"

Target "Clean-Main" (fun _ ->
    CleanDirs [mainBuildDir; mainPackageDir]
)

Target "Clean-Data" (fun _ ->
    CleanDirs [dataBuildDir; dataPackageDir]
)

Target "Clean-TypeScript" (fun _ ->
    CleanDirs [typescriptBuildDir; typescriptPackageDir]
)

Target "Clean-Rx" (fun _ ->
    CleanDirs [rxBuildDir; rxPackageDir]
)

Target "Clean-Test" (fun _ ->
    CleanDirs [testBuildDir]
)

let baseAttributes = [
    Attribute.Product "FunScript"
    Attribute.Company "Type Inferred Ltd."
    Attribute.Copyright "Copyright © 2012-2014 Type Inferred Ltd."
    Attribute.Version versionNumber
    Attribute.FileVersion versionNumber
]


Target "Build-Main" (fun _ ->

    CreateFSharpAssemblyInfo "src/main/FunScript/AssemblyInfo.fs" 
        [
            yield Attribute.Title "TypeInferred.FunScript"
            yield Attribute.Description "An F# to JavaScript Compiler - FunScript"
            yield Attribute.Guid "ABBDBFC5-F6F0-4BB7-89D8-9FE9D105C613"
            yield! baseAttributes
        ]
        
    CreateFSharpAssemblyInfo "src/main/FunScript.Interop/AssemblyInfo.fs" 
        [
            yield Attribute.Title "TypeInferred.FunScript.Interop"
            yield Attribute.Description "FFI Interop Attributes - FunScript"
            yield Attribute.Guid "9A9600A3-CCCD-4DD1-A52A-EA66E0FC2A54"
            yield! baseAttributes
        ]

    let projectFiles = !! "src/main/**/*.fsproj"
    Log "Build-Main-Projects: " projectFiles

    MSBuildRelease mainBuildDir "Build" projectFiles
    |> Log "Build-Main-Output: "
)

Target "Build-Data" (fun _ ->
    
//    RestorePackages()
//    CopyDir dependenciesDir "./packages/" (fun _ -> true)

    CreateFSharpAssemblyInfo "src/data/FunScript.Data/AssemblyInfo.fs" 
        [
            yield Attribute.Title "TypeInferred.FunScript.Data"
            yield Attribute.Description "FSharp.Data Interop Library - FunScript"
            yield Attribute.Guid "38933DD8-EF70-4861-A6CB-E2EA0C4CAE73"
            yield! baseAttributes
        ]
        
    let projectFiles = !! "src/data/**/*.fsproj"
    
    Log "Build-Data-Projects: " projectFiles

    MSBuildRelease dataBuildDir "Build" projectFiles
    |> Log "Build-Data-Output: "
)

Target "Build-TypeScript" (fun _ ->
    
//    RestorePackages()
//    CopyDir dependenciesDir "./packages/" (fun _ -> true)

    CreateFSharpAssemblyInfo "src/extra/FunScript.TypeScript/AssemblyInfo.fs" 
        [
            yield Attribute.Title "TypeInferred.FunScript.TypeScript"
            yield Attribute.Description "TypeScript Interop Library - FunScript"
            yield Attribute.Guid "891C8111-4D9D-45CD-8A8D-77EB817FF8E1"
            yield! baseAttributes
        ]
        
    let projectFiles = !! "src/extra/FunScript.TypeScript/*.fsproj"
    
    Log "Build-TypeScript-Projects: " projectFiles

    MSBuildRelease typescriptBuildDir "Build" projectFiles
    |> Log "Build-TypeScript-Output: "
)

Target "Build-Rx" (fun _ ->
    
//    RestorePackages()
//    CopyDir dependenciesDir "./packages/" (fun _ -> true)

    CreateFSharpAssemblyInfo "src/extra/FunScript.Rx/AssemblyInfo.fs" 
        [
            yield Attribute.Title "TypeInferred.FunScript.Rx"
            yield Attribute.Description "Rx Interop Library - FunScript"
            yield Attribute.Guid "891C8111-4D9D-45CD-8A8D-77EB817FF8E1"
            yield! baseAttributes
        ]
        
    let projectFiles = !! "src/extra/FunScript.Rx/*.fsproj"
    
    Log "Build-Rx-Projects: " projectFiles

    MSBuildRelease rxBuildDir "Build" projectFiles
    |> Log "Build-Rx-Output: "
)

Target "Build-Test" (fun _ ->

    let projectFiles = !! "tests/**/*.fsproj"
    
    Log "Build-Test-Projects: " projectFiles

    MSBuildDebug testBuildDir "Build" projectFiles
    |> Log "Build-Test-Output: "
)

Target "Run-Test" (fun _ ->
    let targetDlls = !! (testBuildDir + "*.Tests.dll")
    //let projectFiles = !! "tests/**/*.fsproj"
    
    Log "Run-Test-Dlls: " targetDlls
   // Log "Run-Test-Dlls: " projectFiles

    targetDlls |> NUnit (fun p -> 
    //projectFiles |> NUnit (fun p -> 
        { p with 
            Framework = "4.5"
            //DisableShadowCopy = true 
           // TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResult.xml"
        })
)

Target "Create-Package-Main" (fun _ ->
    let hasNugetKey = hasBuildParam "nuget_key"
    tracefn "Publish-Package-Main: %b" hasNugetKey
    
    NuGet(fun p ->
        {p with
            Authors = ["Zach Bray"; "Tomas Petricek"]
            Project = "FunScript"
            Summary = "An F# to JavaScript compiler."
            Description = "An F# to JavaScript compiler."
            Copyright = "Copyright © 2012-2014 Type Inferred Ltd."
            WorkingDir = mainBuildDir
            OutputPath = mainPackageDir
            Version = versionNumber
            AccessKey = getBuildParamOrDefault "nuget_key" ""
            Publish = hasNugetKey
        }) "build/template.nuspec"
)

Target "Create-Package-Data" (fun _ ->
    let hasNugetKey = hasBuildParam "nuget_key"
    tracefn "Publish-Package-Main: %b" hasNugetKey
    
    NuGet(fun p ->
        {p with
            Authors = ["Tomas Petricek"]
            Project = "FunScript.Data"
            Summary = "FSharp.Data Type Providers Interop for FunScript."
            Description = "FSharp.Data Type Providers Interop for FunScript."
            Copyright = "Copyright © 2012-2014 Type Inferred Ltd."
            WorkingDir = dataBuildDir
            OutputPath = dataPackageDir
            Version = versionNumber
            AccessKey = getBuildParamOrDefault "nuget_key" ""
            Publish = hasNugetKey
            Dependencies = 
            [
                "FunScript", sprintf "[%s]" versionNumber
                "FSharp.Data", "[2.0.9]"
                "ApiaryProvider", "[1.0.2]"
            ]
        }) "build/data-template.nuspec"
)

Target "Create-Package-Rx" (fun _ ->
    let hasNugetKey = hasBuildParam "nuget_key"
    tracefn "Publish-Package-Rx: %b" hasNugetKey
    
    NuGet(fun p ->
        {p with
            Authors = ["Zach Bray"]
            Project = "FunScript.Rx"
            Summary = "Rx interop. for FunScript."
            Description = "Rx interop. for FunScript."
            Copyright = "Copyright © 2012-2014 Type Inferred Ltd."
            WorkingDir = rxBuildDir
            OutputPath = rxPackageDir
            Version = versionNumber
            AccessKey = getBuildParamOrDefault "nuget_key" ""
            Publish = hasNugetKey
            Dependencies = 
            [
                "FunScript", sprintf "[%s]" versionNumber
                "FSharp.Control.Reactive", "[2.4.0]"
            ]
        }) "build/rx-template.nuspec"
)

Target "Release" DoNothing

"Clean-Main" 
    ==> "Build-Main" 
    ==> "Create-Package-Main" 
    ==> "Release"

"Build-Main" 
    ==> "Build-Data"
    ==> "Build-TypeScript"
    ==> "Build-Rx"
    ==> "Build-Test"

"Clean-Data" 
    ==> "Build-Data" 
    ==> "Create-Package-Data" 
    ==> "Release"

"Clean-Rx" 
    ==> "Build-Rx" 
    ==> "Create-Package-Rx" 
    ==> "Release"

"Clean-Test" 
    ==> "Build-Test" 
    ==> "Run-Test"

"Run-Test" 
    ==> "Create-Package-Main"

RunTargetOrDefault "Release"