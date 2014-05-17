#r @"lib\Fake\FakeLib.dll"

open Fake
open Fake.AssemblyInfoFile

let buildDir = "./build/bin/"
let packageDir = "./build/deploy/"

let versionNumber =
    match buildServer with
    | TeamCity -> buildVersion
    | _ -> "0.0.0"

Target "Clean-Main" (fun _ ->
    CleanDirs [buildDir; packageDir]
)

let baseAttributes = [
    Attribute.Product "FunScript"
    Attribute.Company "Type Inferred Ltd."
    Attribute.Copyright "Copyright © 2012-2014 Type Inferred Ltd."
    Attribute.Version versionNumber
    Attribute.FileVersion versionNumber
]

Target "Build-Main" (fun () ->

    trace "Creating AssemblyInfos"
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

    MSBuildRelease buildDir "Build" projectFiles
    |> Log "Build-Main-Output: "
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
            WorkingDir = buildDir
            OutputPath = packageDir
            Version = versionNumber
            AccessKey = getBuildParamOrDefault "nuget_key" ""
            Publish = hasNugetKey
        }) "build/template.nuspec"
)

Target "Release" DoNothing

"Clean-Main" ==> "Build-Main" ==> "Create-Package-Main" ==> "Release"

RunTargetOrDefault "Release"