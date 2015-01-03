module GoogleClosure

open com.google.javascript.jscomp

/// calls the google closure compiler to optimize/minifiy the generated code
type ClosureCompiler = 
    static member Compile (source, ?filename, ?pretty) = 
        let options = new CompilerOptions()
        options.setPrettyPrint (defaultArg pretty false) // pretty = indented, otherwise minified
        
        let level = CompilationLevel.SIMPLE_OPTIMIZATIONS // ADVANCED_OPTIMIZATIONS may break the application, when using string identifiers
        level.setOptionsForCompilationLevel (options)

        let externs = new java.util.ArrayList() // externs - not yet supported
        let inputs = new java.util.ArrayList() // javascript sources
        inputs.add (SourceFile.fromCode (defaultArg filename "Page.js", source)) |> ignore
        
        let compiler = new Compiler()
        let result = compiler.compile (externs, inputs, options)
        
        if not (result.success) then source
        else compiler.toSource() // fallback to source code if closure fails
