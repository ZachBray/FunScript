#I @"packages\FAKE.1.68.1.0\tools"
#r "FakeLib.dll"
open Fake
 
// Properties
let buildDir = @".\build\"
let nugetDir = @"nugetpackage" 

let appReferences  = !! @"FunScript*\**.fsproj"
 
Target "Clean" (fun _ ->
    CleanDirs [buildDir]
)

Target "BuildApp" (fun _ ->                     
    MSBuildDebug buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)


Target "CreateNuGet" (fun _ -> 
    let nugetLibsDir = nugetDir @@ @"lib\net45"

    if not (System.IO.Directory.Exists nugetLibsDir) then 
        System.IO.Directory.CreateDirectory nugetLibsDir |> ignore

    let dlls = !+ (buildDir @@ "FunScript.*")
    dlls |> Scan |> Seq.iter (fun x -> XCopy x nugetLibsDir)

    XCopy (buildDir @@ @"funsc.exe") nugetLibsDir

    NuGet (fun p -> 
        {p with               
            Authors = ["Zach Bray"; "Tomas Petricek"; "Phillip Trelford"; "James Feiwirth"; "Robert Pickering"]
            Project = "FunScript"
            Version = getBuildParam "version"
            Description = "F# to JavaScript compiler with JQuery etc. mappings through a TypeScript type provider"
            ToolPath = @".\Nuget.exe"
            OutputPath = nugetDir
            AccessKey = getBuildParam "nugetkey"
            Publish = true }) @"FunScript.nuspec"
)

"Clean"
    ==> "BuildApp"
    ==> "CreateNuGet"


RunParameterTargetOrDefault "target" "BuildApp"