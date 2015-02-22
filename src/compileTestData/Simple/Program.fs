[<ReflectedDefinition>]
module Page

module x = 
    let makeHello who = 
        sprintf "hello %s" who

module y = 
    let yMakeHello who = 
            x.makeHello who
    module z =
        let zMakeHello who = 
            x.makeHello who

[<FunScript.AssemblyMain>]
let main() = 
    let hello = y.z.zMakeHello "world"
    hello