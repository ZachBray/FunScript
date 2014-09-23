module FunScript.Core.Enumerator

open FunScript
open System.Collections
open System.Collections.Generic

// NOTE: Literals should not be included in the Reflected Definitions!
module Literals =
    [<Literal>]
    let addArrayEnumerator = """Object.defineProperty({0}, "GetEnumerator", {
	    enumerable: false,
	    configurable: false,
	    writable: false,
	    value: function () {
		    var index = -1;
		    var array = this;
		    return {
			    Dispose:     function() { },
			    MoveNext:    function() { return ++index < array.length },
			    get_Current: function() { return array[index] }
		    }
	    }
    })"""

    [<Literal>]
    let addDicEnumerator = """Object.defineProperty({0}, "GetEnumerator", {
	    enumerable: false,
	    configurable: false,
	    writable: false,
	    value: function () {
		    var index = -1;
		    var dic = this;
		    var keys = Object.keys(dic);
		    return {
			    Dispose:     function() { },
			    MoveNext:    function() { return ++index < keys.length },
			    get_Current: function() {
                    var k = keys[index]
                    return { key: k, value: dic[k] }
			    }
		    }
	    }
    })"""


[<JS; JSEmitInline("{0}.MoveNext()")>]
let NonGenericMoveNext (e: System.Collections.IEnumerator): bool = failwith "never"

[<JS; JSEmitInline("{0}.get_Current()")>]
let NonGenericCurrent (e: System.Collections.IEnumerator): 'a = failwith "never"

[<JS; JSEmitInline("{0}.MoveNext()")>]
let ListMoveNext (e: IEnumerator<'a>): bool = failwith "never"

[<JS; JSEmitInline("{0}.get_Current()")>]
let ListCurrent (e: IEnumerator<'a>): 'a = failwith "never"

[<JS; JSEmitInline("{0}.MoveNext()")>]
let DicMoveNext(e: Dictionary.Enumerator<'k, 'v> ): bool = failwith "never"

[<JS; JSEmitInline("{0}.get_Current()")>]
let DicCurrent(e: Dictionary.Enumerator<'k, 'v> ): KeyValuePair<'k,'v> = failwith "never"

[<JS; JSEmitInline("{0}.MoveNext()")>]
let KeyColMoveNext(e: Dictionary.KeyCollection.Enumerator<'k, 'v> ): bool = failwith "never"

[<JS; JSEmitInline("{0}.get_Current()")>]
let KeyColCurrent(e: Dictionary.KeyCollection.Enumerator<'k, 'v> ): KeyValuePair<'k,'v> = failwith "never"

[<JS; JSEmitInline("{0}.MoveNext()")>]
let ValueColMoveNext(e: Dictionary.ValueCollection.Enumerator<'k, 'v> ): bool = failwith "never"

[<JS; JSEmitInline("{0}.get_Current()")>]
let ValueColCurrent(e: Dictionary.ValueCollection.Enumerator<'k, 'v> ): KeyValuePair<'k,'v> = failwith "never"


[<JS; JSEmit(Literals.addArrayEnumerator)>]
let AddArrayEnumerator (o: obj): unit = failwith "never"

[<JS; JSEmit(Literals.addDicEnumerator)>]
let AddDicEnumerator (o: obj): unit = failwith "never"


