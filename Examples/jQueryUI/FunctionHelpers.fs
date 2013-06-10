[<FunScript.JS>]
module FunScript.FunctionHelpers

type TupledDelegate<'a,'b> = delegate of 'a -> 'b
type TupledDelegate<'a,'b,'c> = delegate of 'a * 'b -> 'c
type TupledDelegate<'a,'b,'c,'d> = delegate of 'a * 'b * 'c -> 'd
type TupledDelegate<'a,'b,'c,'d,'e> = delegate of 'a * 'b * 'c * 'd -> 'e
type TupledDelegate<'a,'b,'c,'d,'e,'f> = delegate of 'a * 'b * 'c * 'd * 'e -> 'f
type TupledDelegate<'a,'b,'c,'d,'e,'f,'g> = delegate of 'a * 'b * 'c * 'd * 'e * 'f -> 'g
type TupledDelegate<'a,'b,'c,'d,'e,'f,'g,'h> = delegate of 'a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h

let immediateFn tupledDelegate = unbox <| (fun () -> tupledDelegate)()

