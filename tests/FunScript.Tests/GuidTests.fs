[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.GuidTests

open System
open NUnit.Framework

[<Test>]
let ``Guid.NewGuid() should be translated``() =
    check 
        <@
            Guid.NewGuid().ToString().Length
        @>

[<Test>]
let ``Guid.Parse should be translated``() =
    let guid = Guid.NewGuid().ToString()
    check 
        <@
            Guid.Parse(guid).ToString()
        @>

[<Test>]
let ``Guid comparison should be translated``() =
    let guidA = Guid.NewGuid().ToString()
    let guidB = Guid.NewGuid().ToString()
    let guidC = Guid.NewGuid().ToString()
    check 
        <@
            let pick (guidA : Guid) guidB = 
                if guidA > guidB then "A"
                else "B"
            let a = Guid.Parse(guidA)
            let b = Guid.Parse(guidB)
            let c = Guid.Parse(guidC)
            pick a b + pick b c + pick a c
        @>