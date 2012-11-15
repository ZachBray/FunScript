module internal FunJS.Seqs

let components = 
   ExpressionReplacer.createModuleMapping 
      "FSharp.Core" "Microsoft.FSharp.Collections.SeqModule"
      "FunJS" "FunJS.Core.Seq"