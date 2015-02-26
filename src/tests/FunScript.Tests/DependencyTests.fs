[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.DependencyTest

open System
open NUnit.Framework

[<ReflectedDefinition>]
module b = 
    let foo() = "bar"

[<ReflectedDefinition>]
module a = 
    let internal x = b.foo()
    let foo() = x

[<Test>]
let ``the variables are injected in the correct order``() =
    check
        <@@
        a.foo()
        @@>
