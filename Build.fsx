#r @"lib\Fake\FakeLib.dll"

open Fake
open Fake.AssemblyInfoFile

let mainBuildDir = "./build/main/bin/"
let mainPackageDir = "./build/main/deploy/"

let dataBuildDir = "./build/data/bin/"
let dataPackageDir = "./build/data/deploy/"

let testBuildDir = "./build/data/bin/"

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

Target "Build-Main" (fun () ->

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

Target "Build-Data" (fun () ->
    
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

Target "Build-Test" (fun () ->

    let projectFiles = !! "src/tests/**/*.fsproj"
    
    Log "Build-Test-Projects: " projectFiles

    MSBuildRelease testBuildDir "Build" projectFiles
    |> Log "Build-Test-Output: "
)

Target "Run-Test" (fun () ->
    let targetDlls = !! (testBuildDir + "/*.Tests.dll")
    
    targetDlls |> NUnit (fun p -> 
        { p with 
            DisableShadowCopy = true 
            OutputFile = "TestResult.xml"
            ToolPath = "lib/NUnit"
        })
)

Target "Create-Package-Main" (fun () ->
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

Target "Create-Package-Data" (fun () ->
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
                "FSharp.Data", "[1.1.5.0]"
                "FSharp.Data.Experimental", "[1.1.5.0]"
            ]
        }) "build/template.nuspec"
)

Target "Release" DoNothing

"Clean-Main" ==> "Build-Main" ==> "Create-Package-Main" ==> "Release"
"Build-Main" ==> "Build-Data"
"Clean-Data" ==> "Build-Data" ==> "Create-Package-Data" ==> "Release"
"Clean-Test" ==> "Build-Test" ==> "Run-Test"
"Run-Test" ==> "Create-Package-Main"

RunTargetOrDefault "Release"