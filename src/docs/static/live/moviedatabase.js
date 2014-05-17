(function () {


Boolean.prototype.CompareTo = function(that) {
   return this - that;
};

Number.prototype.CompareTo = function(that) {
   return this - that;
};

String.prototype.CompareTo = function(that) {
   return this > that
      ? 1
      : this < that
         ? -1
         : 0;
};

Array.prototype.CompareTo = function(that) {
   var i = 0;
   while(i < this.length && i < that.length) {
      var diff = this[i].CompareTo(that[i]);
      if(diff != 0) {
         return diff;
      }
      i = i + 1;
   };
   return this.length - that.length;
};

{
  var list_1_Nil, list_1_Cons, i_mkIEnumerator__2__ctor, i_UnfoldEnumerator_2__ctor, i_OperationArguments__ctor, i_MapIterator_2__ctor, i_KeyValuePair_2__ctor, i_GenericComparer_1__ctor, i_FSharpMap_2__ctor, i_CreateEnumerable_1__ctor, i_CancellationToken__ctor, i_AsyncParams_1__ctor, i_AsyncParamsAux__ctor, i_AsyncBuilder__ctor, i_ApiaryJsContext__ctor, String_splitSingle, String_replaceSingle, String_ToLowerCase, String_Split, String_Replace, String_Join, String_IsNullOrEmpty, String_IndexOf, Seq_Unfold, Seq_TryPickIndexedAux, Seq_TryPickIndexed, Seq_TryFind, Seq_ToArray, Seq_Take, Seq_OfList, Seq_OfArray, Seq_MapIndexed, Seq_Map2, Seq_Map, Seq_Length, Seq_IterateIndexed, Seq_Iterate, Seq_GroupBy, Seq_FromFactory, Seq_FoldIndexedAux, Seq_FoldIndexed, Seq_Fold, Seq_Enumerator, Seq_Delay, Seq_CompareWith, Runtime_setContext, Runtime_newXMLHttpRequest, Runtime_encodeURIComponent, Program_search, Program_op_Dynamic, Program_main, Program_floor, Map_ToSeq, Map_Empty, MapTree_2_MapOne, MapTree_2_MapNode, MapTree_2_MapEmpty, MapTreeModule_tryPick, MapTreeModule_tryFind, MapTreeModule_toList, MapTreeModule_toArray, MapTreeModule_spliceOutSuccessor, MapTreeModule_sizeAux, MapTreeModule_size, MapTreeModule_remove, MapTreeModule_rebalance, MapTreeModule_partitionAux, MapTreeModule_partition1, MapTreeModule_partition, MapTreeModule_notStarted, MapTreeModule_moveNext, MapTreeModule_mkIterator, MapTreeModule_mkIEnumerator, MapTreeModule_mk, MapTreeModule_mem, MapTreeModule_mapi, MapTreeModule_map, MapTreeModule_loop, MapTreeModule_iter, MapTreeModule_isEmpty, MapTreeModule_height, MapTreeModule_forall, MapTreeModule_foldSection, MapTreeModule_foldFromTo, MapTreeModule_foldBack, MapTreeModule_find, MapTreeModule_filterAux, MapTreeModule_filter1, MapTreeModule_filter, MapTreeModule_exists, MapTreeModule_empty, MapTreeModule_current, MapTreeModule_collapseLHS, MapTreeModule_alreadyFinished, MapTreeModule_add, List_ToArray, List_Tail, List_Length, List_IterateIndexed, List_IsEmpty, List_Head, List_FoldIndexedAux, List_FoldIndexed, List_Fold, List_Empty, List_CreateCons, LanguagePrimitives_UnboxGeneric, JsonParser_parse, JsonOperations_TryConvertProperty, JsonOperations_GetText, JsonOperations_GetProperty, JsonOperations_GetInt, JsonOperations_GetFloat, JsonOperations_ConvertArray, JsonDocument_Create, FSharpString_Concat, FSharpMap_2_get_Empty, Async_protectedCont, Async_invokeCont, Async_get_async, Async_StartImmediate, Async_FromContinuations, Async_AwaitJQueryEvent, Async_1_Cont, Array_ZeroCreate, Array_SortInPlaceWith, Array_SortInPlaceBy, Array_SortBy, Array_MapIndexed, Array_Map, Array_Length, Array_Iterate, Array_FoldIndexed, Array_Fold, Array_Copy, Array_ConcatImpl, Array_Concat, Array_Choose, Array_BoxedLength, Array_Append, ApiaryJsRuntime_ProcessParameters, ApiaryJsRuntime_ParseHeaders, ApiaryJsRuntime_AsyncMap, ApiaryJsRuntime_AsyncInvokeOperation, ApiaryJsRuntime_AddQueryParam, ApiaryCompiler_emptyIfNull;
  ApiaryCompiler_emptyIfNull = (function (list)
  {
    return list==null?List_Empty():list;
  });
  ApiaryJsRuntime_AddQueryParam = (function (x, key, value)
  {
    x.GlobalQuery = Array_Append(x.GlobalQuery, [{CompareTo: (function (that)
    {
      var diff;
      return 0.000000;
    }), Item1: key, Item2: value}]);
  });
  ApiaryJsRuntime_AsyncInvokeOperation = (function (x, _arg1)
  {
    var query = _arg1.Query;
    var path = _arg1.Path;
    var meth = _arg1.Method;
    var headers = _arg1.Headers;
    var args = _arg1.Arguments;
    return Async_FromContinuations((function (tupledArg)
    {
      var cont = tupledArg.Item1;
      var econt = tupledArg.Item2;
      var ccont = tupledArg.Item3;
      var allArguments = Array_Append(x.GlobalArguments, args);
      var _temp40;
      var _temp41;
      var folder = (function (_path)
      {
        return (function (_tupledArg)
        {
          var key = _tupledArg.Item1;
          var value = _tupledArg.Item2;
          return String_Replace(_path, key, value);
        });
      });
      _temp41 = (function (source)
      {
        return Seq_Fold(folder, path, Seq_OfArray(source));
      });
      _temp40 = _temp41(allArguments);
      var _path = _temp40;
      var allheaders = Array_Append(headers, x.GlobalHeaders);
      var allquery = Array_Append(query, x.GlobalQuery);
      if ((String_ToLowerCase(meth).CompareTo("get") == 0.000000)) 
      {
        var url = (x.Root + _path);
        var _temp46;
        var _temp47;
        var sep = "\u0026";
        _temp47 = (function (strings)
        {
          return FSharpString_Concat(sep, Seq_OfArray(strings));
        });
        var _temp49;
        var _temp50;
        var mapping = (function (_tupledArg)
        {
          var k = _tupledArg.Item1;
          var v = _tupledArg.Item2;
          return ((k + "=") + Runtime_encodeURIComponent(v));
        });
        _temp50 = (function (array)
        {
          return Array_Map(mapping, array);
        });
        _temp49 = _temp50(allquery);
        _temp46 = _temp47(_temp49);
        var queryString = _temp46;
        var xhr = Runtime_newXMLHttpRequest();
        xhr.open("GET", ((url + "?") + queryString));
        var _temp52;
        var action = (function (_tupledArg)
        {
          var header = _tupledArg.Item1;
          var value = _tupledArg.Item2;
          return xhr.setRequestHeader(header, value);
        });
        _temp52 = (function (array)
        {
          return Array_Iterate(action, array);
        });
        _temp52(allheaders);
        xhr.set_onreadystatechange((function (unitVar0)
        {
          if ((xhr.get_readyState().CompareTo(4.000000) == 0.000000)) 
          {
            var source = xhr.get_responseText();
            var doc = JsonDocument_Create(JsonParser_parse(source));
            Runtime_setContext(doc, (new i_ApiaryJsContext__ctor(x.Root, x.GlobalQuery, x.GlobalHeaders, allArguments)));
            return cont(doc);
          }
          else
          {
            ;
          };
        }));
        return xhr.send("");
      }
      else
      {
        throw ("Only GET supported");
        return null;
      };
    }));
  });
  ApiaryJsRuntime_AsyncMap = (function (work, f)
  {
    return (function (builder_)
    {
      return builder_.Delay((function (unitVar)
      {
        return builder_.Bind(work, (function (_arg2)
        {
          var v = _arg2;
          return builder_.Return(f(v));
        }));
      }));
    })(Async_get_async());
  });
  ApiaryJsRuntime_ParseHeaders = (function (headers)
  {
    var _temp36;
    var chooser = (function (h)
    {
      if (String_IsNullOrEmpty(h)) 
      {
        return {Tag: "None"};
      }
      else
      {
        var arr = String_Split(h, [":"]);
        if ((Array_BoxedLength(arr).CompareTo(2.000000) == 0.000000)) 
        {
          return {Tag: "Some", Value: {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: arr[0.000000], Item2: arr[1.000000]}};
        }
        else
        {
          throw ("Wrong headers");
          return null;
        };
      };
    });
    _temp36 = (function (array)
    {
      return Array_Choose(chooser, array);
    });
    return _temp36(String_Split(headers, ["\n"]));
  });
  ApiaryJsRuntime_ProcessParameters = (function (reqHeaders, headers, query)
  {
    var _headers = Array_Append(ApiaryJsRuntime_ParseHeaders(reqHeaders), List_ToArray(ApiaryCompiler_emptyIfNull(headers)));
    var _query = ApiaryCompiler_emptyIfNull(query);
    return {CompareTo: (function (that)
    {
      var diff;
      return 0.000000;
    }), Item1: _headers, Item2: List_ToArray(_query)};
  });
  Array_Append = (function (xs, ys)
  {
    return xs.concat(ys);
  });
  Array_BoxedLength = (function (xs)
  {
    return xs.length;
  });
  Array_Choose = (function (f, xs)
  {
    var ys = Array_ZeroCreate(0.000000);
    var j = 0.000000;
    for (var _temp35 = 0.000000; _temp35 <= (Array_Length(xs) - 1.000000); _temp35++)
    {
      (function (i)
      {
        var matchValue = f(xs[i]);
        if ((matchValue.Tag == "None")) 
        {
          ;
        }
        else
        {
          var y = matchValue.Value;
          ys[j] = y;
          null;
          j = (j + 1.000000);
          null;
        };
      })(_temp35);
    };
    return ys;
  });
  Array_Concat = (function (xs)
  {
    return (function (xss)
    {
      return Array_ConcatImpl(xss);
    })((function (source)
    {
      return Seq_ToArray(source);
    })(xs));
  });
  Array_ConcatImpl = (function (xss)
  {
    return [].concat.apply([], xss);
  });
  Array_Copy = (function (xs)
  {
    return xs.slice(0);
  });
  Array_Fold = (function (f, seed, xs)
  {
    return Array_FoldIndexed((function (_arg1)
    {
      return (function (acc)
      {
        return (function (x)
        {
          return f(acc)(x);
        });
      });
    }), seed, xs);
  });
  Array_FoldIndexed = (function (f, seed, xs)
  {
    var acc = seed;
    for (var _temp30 = 0.000000; _temp30 <= (Array_Length(xs) - 1.000000); _temp30++)
    {
      (function (i)
      {
        acc = f(i)(acc)(xs[i]);
        null;
      })(_temp30);
    };
    return acc;
  });
  Array_Iterate = (function (f, xs)
  {
    var _temp51;
    return Array_Fold((function (unitVar0)
    {
      return (function (x)
      {
        return f(x);
      });
    }), _temp51, xs);
  });
  Array_Length = (function (xs)
  {
    return xs.length;
  });
  Array_Map = (function (f, xs)
  {
    return Array_MapIndexed((function (_arg1)
    {
      return (function (x)
      {
        return f(x);
      });
    }), xs);
  });
  Array_MapIndexed = (function (f, xs)
  {
    var ys = Array_ZeroCreate(Array_Length(xs));
    for (var _temp26 = 0.000000; _temp26 <= (Array_Length(xs) - 1.000000); _temp26++)
    {
      (function (i)
      {
        ys[i] = f(i)(xs[i]);
        null;
      })(_temp26);
    };
    return ys;
  });
  Array_SortBy = (function (f, xs)
  {
    var ys = Array_Copy(xs);
    Array_SortInPlaceBy(f, ys);
    return ys;
  });
  Array_SortInPlaceBy = (function (f, xs)
  {
    return Array_SortInPlaceWith((function (x)
    {
      return (function (y)
      {
        var _x = f(x);
        var _y = f(y);
        var __x = LanguagePrimitives_UnboxGeneric(_x);
        return __x.CompareTo(_y);
      });
    }), xs);
  });
  Array_SortInPlaceWith = (function (f, xs)
  {
    xs.sort(function(a,b) { return f(a)(b); });
  });
  Array_ZeroCreate = (function (size)
  {
    return new Array(size);
  });
  Async_1_Cont = (function (Item)
  {
    this.Tag = "Cont";
    this.Item = Item;
  });
  Async_AwaitJQueryEvent = (function (f)
  {
    return Async_FromContinuations((function (tupledArg)
    {
      var cont = tupledArg.Item1;
      var econt = tupledArg.Item2;
      var ccont = tupledArg.Item3;
      var named = {contents: {Tag: "None"}};
      named.contents = {Tag: "Some", Value: f((function (v)
      {
        (function (value)
        {
          ;
        })(named.contents.Value.off());
        return cont(v);
      }))};
    }));
  });
  Async_FromContinuations = (function (f)
  {
    return (function (_f)
    {
      return Async_protectedCont(_f);
    })((function (k)
    {
      return f({CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: k.Cont, Item2: k.Aux.ExceptionCont, Item3: k.Aux.CancelledCont});
    }));
  });
  Async_StartImmediate = (function (workflow, cancellationToken)
  {
    var _temp2;
    if ((cancellationToken.Tag == "Some")) 
    {
      var v = cancellationToken.Value;
      _temp2 = v;
    }
    else
    {
      _temp2 = (new i_CancellationToken__ctor({Tag: "None"}));
    };
    var token = _temp2;
    var f = workflow.Item;
    var aux = (new i_AsyncParamsAux__ctor((function (value)
    {
      ;
    }), (function (value)
    {
      ;
    }), token));
    return f((new i_AsyncParams_1__ctor((function (value)
    {
      ;
    }), aux)));
  });
  Async_get_async = (function ()
  {
    return (new i_AsyncBuilder__ctor());
  });
  Async_invokeCont = (function (k, value)
  {
    return k.Cont(value);
  });
  Async_protectedCont = (function (f)
  {
    return (new Async_1_Cont((function (args)
    {
      args.Aux.CancellationToken.ThrowIfCancellationRequested();
      return f(args);
    })));
  });
  FSharpMap_2_get_Empty = (function (unitVar0)
  {
    var comparer = (new i_GenericComparer_1__ctor());
    return (new i_FSharpMap_2__ctor(comparer, (new MapTree_2_MapEmpty())));
  });
  FSharpString_Concat = (function (sep, strings)
  {
    return String_Join(sep, Seq_ToArray(strings));
  });
  JsonDocument_Create = (function (json)
  {
    return json;
  });
  JsonOperations_ConvertArray = (function (doc, f)
  {
    return doc;
  });
  JsonOperations_GetFloat = (function (doc)
  {
    return doc;
  });
  JsonOperations_GetInt = (function (doc)
  {
    return doc;
  });
  JsonOperations_GetProperty = (function (doc, name)
  {
    return doc[name];
  });
  JsonOperations_GetText = (function (doc)
  {
    return doc;
  });
  JsonOperations_TryConvertProperty = (function (doc, name, f)
  {
    
    var it = doc[name];
    return (it == undefined)?{Tag: "None"}:{Tag: "Some", Value: it};
  });
  JsonParser_parse = (function (source)
  {
    var it = source; return (typeof(it)=="string") ? eval("(function(a){return a;})(" + it + ")") : it;
  });
  LanguagePrimitives_UnboxGeneric = (function (x)
  {
    return x;
  });
  List_CreateCons = (function (x, xs)
  {
    return (new list_1_Cons(x, xs));
  });
  List_Empty = (function ()
  {
    return (new list_1_Nil());
  });
  List_Fold = (function (f, seed, xs)
  {
    return List_FoldIndexed((function (_arg1)
    {
      return (function (acc)
      {
        return (function (x)
        {
          return f(acc)(x);
        });
      });
    }), seed, xs);
  });
  List_FoldIndexed = (function (f, seed, xs)
  {
    return List_FoldIndexedAux(f, 0.000000, seed, xs);
  });
  List_FoldIndexedAux = (function (f, i, acc, _arg1)
  {
    if ((_arg1.Tag == "Cons")) 
    {
      var xs = _arg1.Item2;
      var x = _arg1.Item1;
      return List_FoldIndexedAux(f, (i + 1.000000), f(i)(acc)(x), xs);
    }
    else
    {
      return acc;
    };
  });
  List_Head = (function (_arg1)
  {
    if ((_arg1.Tag == "Cons")) 
    {
      var xs = _arg1.Item2;
      var x = _arg1.Item1;
      return x;
    }
    else
    {
      throw ("List was empty");
      return null;
    };
  });
  List_IsEmpty = (function (_arg1)
  {
    return ((_arg1.Tag == "Nil") && true);
  });
  List_IterateIndexed = (function (f, xs)
  {
    var _temp37;
    return List_FoldIndexed((function (i)
    {
      return (function (unitVar1)
      {
        return (function (x)
        {
          return f(i)(x);
        });
      });
    }), _temp37, xs);
  });
  List_Length = (function (xs)
  {
    return List_Fold((function (acc)
    {
      return (function (_arg1)
      {
        return (acc + 1.000000);
      });
    }), 0.000000, xs);
  });
  List_Tail = (function (_arg1)
  {
    if ((_arg1.Tag == "Cons")) 
    {
      var xs = _arg1.Item2;
      var x = _arg1.Item1;
      return xs;
    }
    else
    {
      throw ("List was empty");
      return null;
    };
  });
  List_ToArray = (function (xs)
  {
    var size = List_Length(xs);
    var ys = Array_ZeroCreate(size);
    List_IterateIndexed((function (i)
    {
      return (function (x)
      {
        ys[i] = x;
        return null;
      });
    }), xs);
    return ys;
  });
  MapTreeModule_add = (function (comparer, k, v, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var k2 = m.Item1;
      var c = comparer.Compare(k, k2);
      if ((c.CompareTo(0.000000) < 0.000000)) 
      {
        return (new MapTree_2_MapNode(k, v, (new MapTree_2_MapEmpty()), m, 2.000000));
      }
      else
      {
        if ((c.CompareTo(0.000000) == 0.000000)) 
        {
          return (new MapTree_2_MapOne(k, v));
        }
        else
        {
          return (new MapTree_2_MapNode(k, v, m, (new MapTree_2_MapEmpty()), 2.000000));
        };
      };
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var v2 = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k2 = m.Item1;
        var h = m.Item5;
        var _c = comparer.Compare(k, _k2);
        if ((_c.CompareTo(0.000000) < 0.000000)) 
        {
          return MapTreeModule_rebalance(MapTreeModule_add(comparer, k, v, l), _k2, v2, r);
        }
        else
        {
          if ((_c.CompareTo(0.000000) == 0.000000)) 
          {
            return (new MapTree_2_MapNode(k, v, l, r, h));
          }
          else
          {
            return MapTreeModule_rebalance(l, _k2, v2, MapTreeModule_add(comparer, k, v, r));
          };
        };
      }
      else
      {
        return (new MapTree_2_MapOne(k, v));
      };
    };
  });
  MapTreeModule_alreadyFinished = (function (unitVar0)
  {
    throw ("enumeration already finished");
    return null;
  });
  MapTreeModule_collapseLHS = (function (stack)
  {
    if ((stack.Tag == "Cons")) 
    {
      if ((List_Head(stack).Tag == "MapOne")) 
      {
        return stack;
      }
      else
      {
        if ((List_Head(stack).Tag == "MapNode")) 
        {
          var k = List_Head(stack).Item1;
          var l = List_Head(stack).Item3;
          var r = List_Head(stack).Item4;
          var rest = List_Tail(stack);
          var v = List_Head(stack).Item2;
          return MapTreeModule_collapseLHS(List_CreateCons(l, List_CreateCons((new MapTree_2_MapOne(k, v)), List_CreateCons(r, rest))));
        }
        else
        {
          var _rest = List_Tail(stack);
          return MapTreeModule_collapseLHS(_rest);
        };
      };
    }
    else
    {
      return List_Empty();
    };
  });
  MapTreeModule_current = (function (i)
  {
    if (i.started) 
    {
      var matchValue = i.stack;
      if ((matchValue.Tag == "Empty")) 
      {
        return MapTreeModule_alreadyFinished();
      }
      else
      {
        if ((List_Head(matchValue).Tag == "MapOne")) 
        {
          var k = List_Head(matchValue).Item1;
          var v = List_Head(matchValue).Item2;
          return (new i_KeyValuePair_2__ctor(k, v));
        }
        else
        {
          throw ("Please report error: Map iterator, unexpected stack for current");
          return null;
        };
      };
    }
    else
    {
      return MapTreeModule_notStarted();
    };
  });
  MapTreeModule_empty = (function ()
  {
    return (new MapTree_2_MapEmpty());
  });
  MapTreeModule_exists = (function (f, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v2 = m.Item2;
      var k2 = m.Item1;
      return f(k2)(v2);
    }
    else
    {
      var _temp103;
      var _v2 = m.Item2;
      var r = m.Item4;
      var l = m.Item3;
      var _k2 = m.Item1;
      _temp103 = ((MapTreeModule_exists(f, l) || (function ()
      {
        return f(_k2)(_v2);
      })()) || (function ()
      {
        return MapTreeModule_exists(f, r);
      })());
      return ((m.Tag == "MapNode") && _temp103);
    };
  });
  MapTreeModule_filter = (function (comparer, f, s)
  {
    return MapTreeModule_filterAux(comparer, f, s, MapTreeModule_empty());
  });
  MapTreeModule_filter1 = (function (comparer, f, k, v, acc)
  {
    if (f(k)(v)) 
    {
      return MapTreeModule_add(comparer, k, v, acc);
    }
    else
    {
      return acc;
    };
  });
  MapTreeModule_filterAux = (function (comparer, f, s, acc)
  {
    if ((s.Tag == "MapOne")) 
    {
      var v = s.Item2;
      var k = s.Item1;
      return MapTreeModule_filter1(comparer, f, k, v, acc);
    }
    else
    {
      if ((s.Tag == "MapNode")) 
      {
        var _v = s.Item2;
        var r = s.Item4;
        var l = s.Item3;
        var _k = s.Item1;
        var _acc = MapTreeModule_filterAux(comparer, f, l, acc);
        var __acc = MapTreeModule_filter1(comparer, f, _k, _v, _acc);
        return MapTreeModule_filterAux(comparer, f, r, __acc);
      }
      else
      {
        return acc;
      };
    };
  });
  MapTreeModule_find = (function (comparer, k, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v2 = m.Item2;
      var k2 = m.Item1;
      var c = comparer.Compare(k, k2);
      if ((c.CompareTo(0.000000) == 0.000000)) 
      {
        return v2;
      }
      else
      {
        throw ("key not found");
        return null;
      };
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v2 = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k2 = m.Item1;
        var _c = comparer.Compare(k, _k2);
        if ((_c.CompareTo(0.000000) < 0.000000)) 
        {
          return MapTreeModule_find(comparer, k, l);
        }
        else
        {
          if ((_c.CompareTo(0.000000) == 0.000000)) 
          {
            return _v2;
          }
          else
          {
            return MapTreeModule_find(comparer, k, r);
          };
        };
      }
      else
      {
        throw ("key not found");
        return null;
      };
    };
  });
  MapTreeModule_foldBack = (function (f, m, x)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v = m.Item2;
      var k = m.Item1;
      return f(k)(v)(x);
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k = m.Item1;
        var _x = MapTreeModule_foldBack(f, r, x);
        var __x = f(_k)(_v)(_x);
        return MapTreeModule_foldBack(f, l, __x);
      }
      else
      {
        return x;
      };
    };
  });
  MapTreeModule_foldFromTo = (function (comparer, lo, hi, f, m, x)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v = m.Item2;
      var k = m.Item1;
      var cLoKey = comparer.Compare(lo, k);
      var cKeyHi = comparer.Compare(k, hi);
      var _temp104;
      if (((cLoKey.CompareTo(0.000000) <= 0.000000) && (cKeyHi.CompareTo(0.000000) <= 0.000000))) 
      {
        _temp104 = f(k)(v)(x);
      }
      else
      {
        _temp104 = x;
      };
      var _x = _temp104;
      return _x;
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k = m.Item1;
        var _cLoKey = comparer.Compare(lo, _k);
        var _cKeyHi = comparer.Compare(_k, hi);
        var _temp105;
        if ((_cLoKey.CompareTo(0.000000) < 0.000000)) 
        {
          _temp105 = MapTreeModule_foldFromTo(comparer, lo, hi, f, l, x);
        }
        else
        {
          _temp105 = x;
        };
        var __x = _temp105;
        var _temp106;
        if (((_cLoKey.CompareTo(0.000000) <= 0.000000) && (_cKeyHi.CompareTo(0.000000) <= 0.000000))) 
        {
          _temp106 = f(_k)(_v)(__x);
        }
        else
        {
          _temp106 = __x;
        };
        var ___x = _temp106;
        var _temp107;
        if ((_cKeyHi.CompareTo(0.000000) < 0.000000)) 
        {
          _temp107 = MapTreeModule_foldFromTo(comparer, lo, hi, f, r, ___x);
        }
        else
        {
          _temp107 = ___x;
        };
        var ____x = _temp107;
        return ____x;
      }
      else
      {
        return x;
      };
    };
  });
  MapTreeModule_foldSection = (function (comparer, lo, hi, f, m, x)
  {
    if ((comparer.Compare(lo, hi).CompareTo(1.000000) == 0.000000)) 
    {
      return x;
    }
    else
    {
      return MapTreeModule_foldFromTo(comparer, lo, hi, f, m, x);
    };
  });
  MapTreeModule_forall = (function (f, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v2 = m.Item2;
      var k2 = m.Item1;
      return f(k2)(v2);
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v2 = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k2 = m.Item1;
        return ((MapTreeModule_forall(f, l) && f(_k2)(_v2)) && MapTreeModule_forall(f, r));
      }
      else
      {
        return true;
      };
    };
  });
  MapTreeModule_height = (function (_arg1)
  {
    if ((_arg1.Tag == "MapOne")) 
    {
      return 1.000000;
    }
    else
    {
      if ((_arg1.Tag == "MapNode")) 
      {
        var h = _arg1.Item5;
        return h;
      }
      else
      {
        return 0.000000;
      };
    };
  });
  MapTreeModule_isEmpty = (function (m)
  {
    return ((m.Tag == "MapEmpty") && true);
  });
  MapTreeModule_iter = (function (f, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v2 = m.Item2;
      var k2 = m.Item1;
      return f(k2)(v2);
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v2 = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k2 = m.Item1;
        MapTreeModule_iter(f, l);
        f(_k2)(_v2);
        return MapTreeModule_iter(f, r);
      }
      else
      {
        ;
      };
    };
  });
  MapTreeModule_loop = (function (m, acc)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v = m.Item2;
      var k = m.Item1;
      return List_CreateCons({CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: k, Item2: v}, acc);
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k = m.Item1;
        return MapTreeModule_loop(l, List_CreateCons({CompareTo: (function (that)
        {
          var diff;
          return 0.000000;
        }), Item1: _k, Item2: _v}, MapTreeModule_loop(r, acc)));
      }
      else
      {
        return acc;
      };
    };
  });
  MapTreeModule_map = (function (f, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v = m.Item2;
      var k = m.Item1;
      return (new MapTree_2_MapOne(k, f(v)));
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k = m.Item1;
        var h = m.Item5;
        var l2 = MapTreeModule_map(f, l);
        var v2 = f(_v);
        var r2 = MapTreeModule_map(f, r);
        return (new MapTree_2_MapNode(_k, v2, l2, r2, h));
      }
      else
      {
        return MapTreeModule_empty();
      };
    };
  });
  MapTreeModule_mapi = (function (f, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v = m.Item2;
      var k = m.Item1;
      return (new MapTree_2_MapOne(k, f(k)(v)));
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k = m.Item1;
        var h = m.Item5;
        var l2 = MapTreeModule_mapi(f, l);
        var v2 = f(_k)(_v);
        var r2 = MapTreeModule_mapi(f, r);
        return (new MapTree_2_MapNode(_k, v2, l2, r2, h));
      }
      else
      {
        return MapTreeModule_empty();
      };
    };
  });
  MapTreeModule_mem = (function (comparer, k, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var k2 = m.Item1;
      return (comparer.Compare(k, k2).CompareTo(0.000000) == 0.000000);
    }
    else
    {
      var _temp110;
      var r = m.Item4;
      var l = m.Item3;
      var _k2 = m.Item1;
      var c = comparer.Compare(k, _k2);
      if ((c.CompareTo(0.000000) < 0.000000)) 
      {
        _temp110 = MapTreeModule_mem(comparer, k, l);
      }
      else
      {
        _temp110 = ((c.CompareTo(0.000000) == 0.000000) || (function ()
        {
          return MapTreeModule_mem(comparer, k, r);
        })());
      };
      return ((m.Tag == "MapNode") && _temp110);
    };
  });
  MapTreeModule_mk = (function (l, k, v, r)
  {
    var matchValue = {CompareTo: (function (that)
    {
      var diff;
      return 0.000000;
    }), Item1: l, Item2: r};
    if ((matchValue.Item1.Tag == "MapEmpty")) 
    {
      if ((matchValue.Item2.Tag == "MapEmpty")) 
      {
        return (new MapTree_2_MapOne(k, v));
      }
      else
      {
        var hl = MapTreeModule_height(l);
        var hr = MapTreeModule_height(r);
        var _temp101;
        if ((hl.CompareTo(hr) < 0.000000)) 
        {
          _temp101 = hr;
        }
        else
        {
          _temp101 = hl;
        };
        var m = _temp101;
        return (new MapTree_2_MapNode(k, v, l, r, (m + 1.000000)));
      };
    }
    else
    {
      var _hl = MapTreeModule_height(l);
      var _hr = MapTreeModule_height(r);
      var _temp102;
      if ((_hl.CompareTo(_hr) < 0.000000)) 
      {
        _temp102 = _hr;
      }
      else
      {
        _temp102 = _hl;
      };
      var _m = _temp102;
      return (new MapTree_2_MapNode(k, v, l, r, (_m + 1.000000)));
    };
  });
  MapTreeModule_mkIEnumerator = (function (s)
  {
    return (new i_mkIEnumerator__2__ctor(s));
  });
  MapTreeModule_mkIterator = (function (s)
  {
    return (new i_MapIterator_2__ctor(MapTreeModule_collapseLHS(List_CreateCons(s, List_Empty())), false));
  });
  MapTreeModule_moveNext = (function (i)
  {
    if (i.started) 
    {
      var matchValue = i.stack;
      if ((matchValue.Tag == "Empty")) 
      {
        return false;
      }
      else
      {
        if ((List_Head(matchValue).Tag == "MapOne")) 
        {
          var rest = List_Tail(matchValue);
          i.stack = MapTreeModule_collapseLHS(rest);
          null;
          return (!List_IsEmpty(i.stack));
        }
        else
        {
          throw ("Please report error: Map iterator, unexpected stack for moveNext");
          return null;
        };
      };
    }
    else
    {
      i.started = true;
      null;
      return (!List_IsEmpty(i.stack));
    };
  });
  MapTreeModule_notStarted = (function (unitVar0)
  {
    throw ("enumeration not started");
    return null;
  });
  MapTreeModule_partition = (function (comparer, f, s)
  {
    return MapTreeModule_partitionAux(comparer, f, s, MapTreeModule_empty(), MapTreeModule_empty());
  });
  MapTreeModule_partition1 = (function (comparer, f, k, v, acc1, acc2)
  {
    if (f(k)(v)) 
    {
      return {CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: MapTreeModule_add(comparer, k, v, acc1), Item2: acc2};
    }
    else
    {
      return {CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: acc1, Item2: MapTreeModule_add(comparer, k, v, acc2)};
    };
  });
  MapTreeModule_partitionAux = (function (comparer, f, s, acc_0, acc_1)
  {
    var acc = {CompareTo: (function (that)
    {
      var diff;
      return 0.000000;
    }), Item1: acc_0, Item2: acc_1};
    if ((s.Tag == "MapOne")) 
    {
      var v = s.Item2;
      var k = s.Item1;
      var acc1 = acc.Item1;
      var acc2 = acc.Item2;
      return MapTreeModule_partition1(comparer, f, k, v, acc1, acc2);
    }
    else
    {
      if ((s.Tag == "MapNode")) 
      {
        var _v = s.Item2;
        var r = s.Item4;
        var l = s.Item3;
        var _k = s.Item1;
        var _temp108;
        var arg30_ = acc.Item1;
        var arg31_ = acc.Item2;
        _temp108 = MapTreeModule_partitionAux(comparer, f, r, arg30_, arg31_);
        var _acc = _temp108;
        var _temp109;
        var _acc1 = _acc.Item1;
        var _acc2 = _acc.Item2;
        _temp109 = MapTreeModule_partition1(comparer, f, _k, _v, _acc1, _acc2);
        var __acc = _temp109;
        var _arg30_ = __acc.Item1;
        var _arg31_ = __acc.Item2;
        return MapTreeModule_partitionAux(comparer, f, l, _arg30_, _arg31_);
      }
      else
      {
        return acc;
      };
    };
  });
  MapTreeModule_rebalance = (function (t1, k, v, t2)
  {
    var t1h = MapTreeModule_height(t1);
    var t2h = MapTreeModule_height(t2);
    if ((t2h.CompareTo((t1h + 2.000000)) > 0.000000)) 
    {
      if ((t2.Tag == "MapNode")) 
      {
        var t2v = t2.Item2;
        var t2r = t2.Item4;
        var t2l = t2.Item3;
        var t2k = t2.Item1;
        if ((MapTreeModule_height(t2l).CompareTo((t1h + 1.000000)) > 0.000000)) 
        {
          if ((t2l.Tag == "MapNode")) 
          {
            var t2lv = t2l.Item2;
            var t2lr = t2l.Item4;
            var t2ll = t2l.Item3;
            var t2lk = t2l.Item1;
            return MapTreeModule_mk(MapTreeModule_mk(t1, k, v, t2ll), t2lk, t2lv, MapTreeModule_mk(t2lr, t2k, t2v, t2r));
          }
          else
          {
            throw ("rebalance");
            return null;
          };
        }
        else
        {
          return MapTreeModule_mk(MapTreeModule_mk(t1, k, v, t2l), t2k, t2v, t2r);
        };
      }
      else
      {
        throw ("rebalance");
        return null;
      };
    }
    else
    {
      if ((t1h.CompareTo((t2h + 2.000000)) > 0.000000)) 
      {
        if ((t1.Tag == "MapNode")) 
        {
          var t1v = t1.Item2;
          var t1r = t1.Item4;
          var t1l = t1.Item3;
          var t1k = t1.Item1;
          if ((MapTreeModule_height(t1r).CompareTo((t2h + 1.000000)) > 0.000000)) 
          {
            if ((t1r.Tag == "MapNode")) 
            {
              var t1rv = t1r.Item2;
              var t1rr = t1r.Item4;
              var t1rl = t1r.Item3;
              var t1rk = t1r.Item1;
              return MapTreeModule_mk(MapTreeModule_mk(t1l, t1k, t1v, t1rl), t1rk, t1rv, MapTreeModule_mk(t1rr, k, v, t2));
            }
            else
            {
              throw ("re  balance");
              return null;
            };
          }
          else
          {
            return MapTreeModule_mk(t1l, t1k, t1v, MapTreeModule_mk(t1r, k, v, t2));
          };
        }
        else
        {
          throw ("rebalance");
          return null;
        };
      }
      else
      {
        return MapTreeModule_mk(t1, k, v, t2);
      };
    };
  });
  MapTreeModule_remove = (function (comparer, k, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var k2 = m.Item1;
      var c = comparer.Compare(k, k2);
      if ((c.CompareTo(0.000000) == 0.000000)) 
      {
        return (new MapTree_2_MapEmpty());
      }
      else
      {
        return m;
      };
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var v2 = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k2 = m.Item1;
        var _c = comparer.Compare(k, _k2);
        if ((_c.CompareTo(0.000000) < 0.000000)) 
        {
          return MapTreeModule_rebalance(MapTreeModule_remove(comparer, k, l), _k2, v2, r);
        }
        else
        {
          if ((_c.CompareTo(0.000000) == 0.000000)) 
          {
            var matchValue = {CompareTo: (function (that)
            {
              var diff;
              return 0.000000;
            }), Item1: l, Item2: r};
            if ((matchValue.Item1.Tag == "MapEmpty")) 
            {
              return r;
            }
            else
            {
              if ((matchValue.Item2.Tag == "MapEmpty")) 
              {
                return l;
              }
              else
              {
                var patternInput = MapTreeModule_spliceOutSuccessor(r);
                var sv = patternInput.Item2;
                var sk = patternInput.Item1;
                var r_ = patternInput.Item3;
                return MapTreeModule_mk(l, sk, sv, r_);
              };
            };
          }
          else
          {
            return MapTreeModule_rebalance(l, _k2, v2, MapTreeModule_remove(comparer, k, r));
          };
        };
      }
      else
      {
        return MapTreeModule_empty();
      };
    };
  });
  MapTreeModule_size = (function (x)
  {
    return MapTreeModule_sizeAux(0.000000, x);
  });
  MapTreeModule_sizeAux = (function (acc, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      return (acc + 1.000000);
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var r = m.Item4;
        var l = m.Item3;
        return MapTreeModule_sizeAux(MapTreeModule_sizeAux((acc + 1.000000), l), r);
      }
      else
      {
        return acc;
      };
    };
  });
  MapTreeModule_spliceOutSuccessor = (function (m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v2 = m.Item2;
      var k2 = m.Item1;
      return {CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: k2, Item2: v2, Item3: (new MapTree_2_MapEmpty())};
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v2 = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k2 = m.Item1;
        if ((l.Tag == "MapEmpty")) 
        {
          return {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: _k2, Item2: _v2, Item3: r};
        }
        else
        {
          var patternInput = MapTreeModule_spliceOutSuccessor(l);
          var v3 = patternInput.Item2;
          var l_ = patternInput.Item3;
          var k3 = patternInput.Item1;
          return {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: k3, Item2: v3, Item3: MapTreeModule_mk(l_, _k2, _v2, r)};
        };
      }
      else
      {
        throw ("internal error: Map.spliceOutSuccessor");
        return null;
      };
    };
  });
  MapTreeModule_toArray = (function (m)
  {
    return (function (list)
    {
      return List_ToArray(list);
    })((function (_m)
    {
      return MapTreeModule_toList(_m);
    })(m));
  });
  MapTreeModule_toList = (function (m)
  {
    return MapTreeModule_loop(m, List_Empty());
  });
  MapTreeModule_tryFind = (function (comparer, k, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v2 = m.Item2;
      var k2 = m.Item1;
      var c = comparer.Compare(k, k2);
      if ((c.CompareTo(0.000000) == 0.000000)) 
      {
        return {Tag: "Some", Value: v2};
      }
      else
      {
        return {Tag: "None"};
      };
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v2 = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k2 = m.Item1;
        var _c = comparer.Compare(k, _k2);
        if ((_c.CompareTo(0.000000) < 0.000000)) 
        {
          return MapTreeModule_tryFind(comparer, k, l);
        }
        else
        {
          if ((_c.CompareTo(0.000000) == 0.000000)) 
          {
            return {Tag: "Some", Value: _v2};
          }
          else
          {
            return MapTreeModule_tryFind(comparer, k, r);
          };
        };
      }
      else
      {
        return {Tag: "None"};
      };
    };
  });
  MapTreeModule_tryPick = (function (f, m)
  {
    if ((m.Tag == "MapOne")) 
    {
      var v2 = m.Item2;
      var k2 = m.Item1;
      return f(k2)(v2);
    }
    else
    {
      if ((m.Tag == "MapNode")) 
      {
        var _v2 = m.Item2;
        var r = m.Item4;
        var l = m.Item3;
        var _k2 = m.Item1;
        var matchValue = MapTreeModule_tryPick(f, l);
        if ((matchValue.Tag == "None")) 
        {
          var _matchValue = f(_k2)(_v2);
          if ((_matchValue.Tag == "None")) 
          {
            return MapTreeModule_tryPick(f, r);
          }
          else
          {
            var res = _matchValue;
            return res;
          };
        }
        else
        {
          var _res = matchValue;
          return _res;
        };
      }
      else
      {
        return {Tag: "None"};
      };
    };
  });
  MapTree_2_MapEmpty = (function ()
  {
    this.Tag = "MapEmpty";
  });
  MapTree_2_MapNode = (function (Item1, Item2, Item3, Item4, Item5)
  {
    this.Tag = "MapNode";
    this.Item1 = Item1;
    this.Item2 = Item2;
    this.Item3 = Item3;
    this.Item4 = Item4;
    this.Item5 = Item5;
  });
  MapTree_2_MapOne = (function (Item1, Item2)
  {
    this.Tag = "MapOne";
    this.Item1 = Item1;
    this.Item2 = Item2;
  });
  Map_Empty = (function ()
  {
    return FSharpMap_2_get_Empty();
  });
  Map_ToSeq = (function (m)
  {
    var _temp100;
    var mapping = (function (kvp)
    {
      return {CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: kvp.get_Key(), Item2: kvp.get_Value()};
    });
    _temp100 = (function (source)
    {
      return Seq_Map(mapping, source);
    });
    return _temp100(m);
  });
  Program_floor = (function (n)
  {
    return Math.floor(n);
  });
  Program_main = (function (unitVar0)
  {
    return (function (arg00)
    {
      return Async_StartImmediate(arg00, {Tag: "None"});
    })((function (builder_)
    {
      return builder_.Delay((function (unitVar)
      {
        return builder_.Bind(Async_AwaitJQueryEvent((function (a)
        {
          return Program_op_Dynamic(jQuery, "document").ready(a);
        })), (function (_arg1)
        {
          return builder_.While((function (_unitVar)
          {
            return true;
          }), builder_.Delay((function (_unitVar)
          {
            var _temp4;
            var _temp5;
            var objectArg = Program_op_Dynamic(jQuery, "searchButton");
            _temp5 = (function (arg00)
            {
              return objectArg.click(arg00);
            });
            _temp4 = Async_AwaitJQueryEvent(_temp5);
            return builder_.Bind(_temp4, (function (_arg2)
            {
              var evt = _arg2;
              var id = LanguagePrimitives_UnboxGeneric(Program_op_Dynamic(jQuery, "searchInput").val());
              return builder_.Bind(Program_search(id), (function (_arg3)
              {
                var _temp353;
                return builder_.Return(_temp353);
              }));
            }));
          })));
        }));
      }));
    })(Async_get_async()));
  });
  Program_op_Dynamic = (function (jq, name)
  {
    if ((name.CompareTo("document") != 0.000000)) 
    {
      return jq(("#" + name));
    }
    else
    {
      return jq(name);
    };
  });
  Program_search = (function (term)
  {
    return (function (builder_)
    {
      return builder_.Delay((function (unitVar)
      {
        var _temp6;
        var var0 = "http://api.themoviedb.org";
        _temp6 = {Root: var0, GlobalQuery: [], GlobalHeaders: [], GlobalArguments: []};
        var db = _temp6;
        ApiaryJsRuntime_AddQueryParam(db, "api_key", "6ce0ef5b176501f8c07c634dfa933cff");
        var root = "http://cf2.imgobject.com/t/p/w92/";
        var showDetails = (function (id)
        {
          return (function (_builder_)
          {
            return _builder_.Delay((function (_unitVar)
            {
              var _temp64;
              var _var0 = db;
              var var1 = id.toString();
              var var2 = null;
              var var3 = null;
              var _temp66;
              var _temp67;
              var patternInput = ApiaryJsRuntime_ProcessParameters("Accept:application/json", var3, var2);
              var query = patternInput.Item2;
              var headers = patternInput.Item1;
              _temp67 = {CompareTo: (function (that)
              {
                var diff;
                return 0.000000;
              }), Item1: _var0, Item2: (new i_OperationArguments__ctor("GET", "/3/movie/{id}", [{CompareTo: (function (that)
              {
                var diff;
                return 0.000000;
              }), Item1: "{id}", Item2: var1}], headers, query))};
              var _patternInput = _temp67;
              var argszz = _patternInput.Item2;
              var apiCtx = _patternInput.Item1;
              _temp66 = ApiaryJsRuntime_AsyncInvokeOperation(apiCtx, argszz);
              _temp64 = ApiaryJsRuntime_AsyncMap(_temp66, (function (doc)
              {
                return doc;
              }));
              return _builder_.Bind(_temp64, (function (_arg1)
              {
                var movie = _arg1;
                (function (value)
                {
                  ;
                })(Program_op_Dynamic(jQuery, "dialogOverview").text(JsonOperations_GetText(JsonOperations_GetProperty(movie, "overview"))));
                (function (value)
                {
                  ;
                })(Program_op_Dynamic(jQuery, "dialogTitle").text(JsonOperations_GetText(JsonOperations_GetProperty(movie, "title"))));
                (function (value)
                {
                  ;
                })(Program_op_Dynamic(jQuery, "dialogImage").attr("src", (root + JsonOperations_GetText(JsonOperations_GetProperty(movie, "poster_path")))));
                var _temp71;
                var _var1 = null;
                var _var2 = null;
                var _temp73;
                var _temp74;
                var __patternInput = ApiaryJsRuntime_ProcessParameters("Accept:application/json", _var2, _var1);
                var _query = __patternInput.Item2;
                var _headers = __patternInput.Item1;
                var _apiCtx = LanguagePrimitives_UnboxGeneric(movie.get_Context());
                _temp74 = {CompareTo: (function (that)
                {
                  var diff;
                  return 0.000000;
                }), Item1: _apiCtx, Item2: (new i_OperationArguments__ctor("GET", "/3/movie/{id}/casts", [], _headers, _query))};
                var ___patternInput = _temp74;
                var _argszz = ___patternInput.Item2;
                var __apiCtx = ___patternInput.Item1;
                _temp73 = ApiaryJsRuntime_AsyncInvokeOperation(__apiCtx, _argszz);
                _temp71 = ApiaryJsRuntime_AsyncMap(_temp73, (function (doc)
                {
                  return doc;
                }));
                return _builder_.Bind(_temp71, (function (_arg2)
                {
                  var casts = _arg2;
                  (function (value)
                  {
                    ;
                  })(Program_op_Dynamic(jQuery, "dialogCast").html(""));
                  var _temp76;
                  var _temp77;
                  var projection = (function (c)
                  {
                    return JsonOperations_GetInt(JsonOperations_GetProperty(c, "order"));
                  });
                  _temp77 = (function (array)
                  {
                    return Array_SortBy(projection, array);
                  });
                  _temp76 = _temp77(JsonOperations_ConvertArray(JsonOperations_GetProperty(casts, "cast"), (function (x)
                  {
                    return x;
                  })));
                  var sorted = _temp76;
                  var _temp80;
                  if ((Array_BoxedLength(sorted).CompareTo(10.000000) <= 0.000000)) 
                  {
                    _temp80 = (function (source)
                    {
                      return Seq_OfArray(source);
                    })(sorted);
                  }
                  else
                  {
                    var _temp81;
                    var count = 10.000000;
                    _temp81 = (function (source)
                    {
                      return Seq_Take(count, source);
                    });
                    _temp80 = _temp81((function (source)
                    {
                      return Seq_OfArray(source);
                    })(sorted));
                  };
                  var _sorted = _temp80;
                  return _builder_.For(Seq_OfArray(JsonOperations_ConvertArray(JsonOperations_GetProperty(casts, "cast"), (function (x)
                  {
                    return x;
                  }))), (function (_arg3)
                  {
                    var cast = _arg3;
                    var html = (((("\u003cstrong\u003e" + JsonOperations_GetText(JsonOperations_GetProperty(cast, "name"))) + "\u003c/strong\u003e (") + JsonOperations_GetText(JsonOperations_GetProperty(cast, "character"))) + ")");
                    (function (value)
                    {
                      ;
                    })(jQuery("\u003cli\u003e").html(html).appendTo(Program_op_Dynamic(jQuery, "dialogCast")));
                    return _builder_.Zero();
                  }));
                }));
              }));
            }));
          })(Async_get_async());
        });
        var _temp85;
        var _var0 = db;
        var var1 = List_CreateCons({CompareTo: (function (that)
        {
          var diff;
          return 0.000000;
        }), Item1: "query", Item2: term}, List_Empty());
        var var2 = null;
        var _temp87;
        var _temp88;
        var patternInput = ApiaryJsRuntime_ProcessParameters("Accept:application/json", var2, var1);
        var query = patternInput.Item2;
        var headers = patternInput.Item1;
        _temp88 = {CompareTo: (function (that)
        {
          var diff;
          return 0.000000;
        }), Item1: _var0, Item2: (new i_OperationArguments__ctor("GET", "/3/search/movie", [], headers, query))};
        var _patternInput = _temp88;
        var argszz = _patternInput.Item2;
        var apiCtx = _patternInput.Item1;
        _temp87 = ApiaryJsRuntime_AsyncInvokeOperation(apiCtx, argszz);
        _temp85 = ApiaryJsRuntime_AsyncMap(_temp87, (function (doc)
        {
          return doc;
        }));
        return builder_.Bind(_temp85, (function (_arg4)
        {
          var res = _arg4;
          (function (value)
          {
            ;
          })(Program_op_Dynamic(jQuery, "results").html(""));
          var _temp93;
          var action = (function (tupledArg)
          {
            var _arg3 = tupledArg.Item1;
            var group = tupledArg.Item2;
            var _temp95;
            var _temp96;
            var mapping = (function (_tupledArg)
            {
              var _arg2 = _tupledArg.Item1;
              var v = _tupledArg.Item2;
              return v;
            });
            _temp96 = (function (array)
            {
              return Array_Map(mapping, array);
            });
            _temp95 = _temp96((function (source)
            {
              return Seq_ToArray(source);
            })(group));
            var elems = _temp95;
            return (function (value)
            {
              ;
            })(jQuery("\u003cdiv\u003e").addClass("row-fluid").append(elems).appendTo(Program_op_Dynamic(jQuery, "results")));
          });
          _temp93 = (function (source)
          {
            return Seq_Iterate(action, source);
          });
          var _temp232;
          var _temp233;
          var projection = (function (tuple)
          {
            return tuple.Item1;
          });
          _temp233 = (function (source)
          {
            return Seq_GroupBy(projection, source);
          });
          var _temp293;
          var _temp323;
          var mapping = (function (index)
          {
            return (function (item)
            {
              var _temp338;
              var _temp342;
              var _temp344;
              var _temp345;
              var copyOfStruct = JsonOperations_GetFloat(JsonOperations_GetProperty(item, "vote_average"));
              _temp345 = copyOfStruct.toString();
              _temp344 = ("\u003cstrong\u003eAverage vote:\u003c/strong\u003e " + _temp345);
              _temp342 = jQuery("\u003cli\u003e").html(_temp344);
              var _temp349;
              var _temp351;
              var _temp352;
              var _copyOfStruct = JsonOperations_GetFloat(JsonOperations_GetProperty(item, "popularity"));
              _temp352 = _copyOfStruct.toString();
              _temp351 = ("\u003cstrong\u003ePopularity:\u003c/strong\u003e " + _temp352);
              _temp349 = jQuery("\u003cli\u003e").html(_temp351);
              _temp338 = [jQuery("\u003cli\u003e").html(("\u003cstrong\u003eReleased:\u003c/strong\u003e " + JsonOperations_TryConvertProperty(item, "release_date", (function (x)
              {
                return JsonOperations_GetText(x);
              })).Value)), _temp342, _temp349];
              var details = _temp338;
              var link = jQuery("\u003ca\u003e").attr("data-toggle", "modal").attr("href", "#detailsDialog").text(JsonOperations_GetText(JsonOperations_GetProperty(item, "title"))).click((function (_arg1)
              {
                return (function (arg00)
                {
                  return Async_StartImmediate(arg00, {Tag: "None"});
                })(showDetails(JsonOperations_GetInt(JsonOperations_GetProperty(item, "id"))));
              }));
              var body = [jQuery("\u003ch3\u003e").append([link]), jQuery("\u003cimg\u003e").attr("src", (root + JsonOperations_TryConvertProperty(item, "poster_path", (function (x)
              {
                return JsonOperations_GetText(x);
              })).Value)), jQuery("\u003cul\u003e").append(details), jQuery("\u003cdiv\u003e").addClass("clearer")];
              return {CompareTo: (function (that)
              {
                var diff;
                return 0.000000;
              }), Item1: Program_floor((index / 3.000000)), Item2: jQuery("\u003cdiv\u003e").addClass("searchResult").addClass("span4").append(body)};
            });
          });
          _temp323 = (function (source)
          {
            return Seq_MapIndexed(mapping, source);
          });
          _temp293 = _temp323((function (source)
          {
            return Seq_OfArray(source);
          })(JsonOperations_ConvertArray(JsonOperations_GetProperty(res, "results"), (function (x)
          {
            return x;
          }))));
          _temp232 = _temp233(_temp293);
          _temp93(_temp232);
          return builder_.Zero();
        }));
      }));
    })(Async_get_async());
  });
  Runtime_encodeURIComponent = (function (s)
  {
    return encodeURIComponent(s);
  });
  Runtime_newXMLHttpRequest = (function (unitVar0)
  {
    
      var res;
      if (window.XDomainRequest) {
        res = new XDomainRequest();
        res.setRequestHeader = function (header, value) { };
        res.onload = function () {
          res.readyState = 4;
          res.status = 200;
          res.onreadystatechange();
        };
      }
      else if (window.XMLHttpRequest)
        res = new XMLHttpRequest();
      else
        res = new ActiveXObject("Microsoft.XMLHTTP");
      res.get_onreadystatechange = function() { return res.onreadystatechange; }
      res.set_onreadystatechange = function(a) { res.onreadystatechange = a; }
      res.get_readyState = function() { return res.readyState; }
      res.get_responseText = function() { return res.responseText; }
      return res;
  });
  Runtime_setContext = (function (o, ctx)
  {
    o.get_Context = function() { return ctx; };
  });
  Seq_CompareWith = (function (f, xs, ys)
  {
    var _temp20;
    var _temp21;
    var _f = (function (i)
    {
      return (i.CompareTo(0.000000) != 0.000000);
    });
    _temp21 = (function (_xs)
    {
      return Seq_TryFind(_f, _xs);
    });
    _temp20 = _temp21(Seq_Map2((function (x)
    {
      return (function (y)
      {
        return f(x)(y);
      });
    }), xs, ys));
    var nonZero = _temp20;
    if ((nonZero.Tag == "None")) 
    {
      return (Seq_Length(xs) - Seq_Length(ys));
    }
    else
    {
      var diff = nonZero.Value;
      return diff;
    };
  });
  Seq_Delay = (function (f)
  {
    return Seq_FromFactory((function (unitVar0)
    {
      var _temp10;
      var _temp11;
      _temp10 = f(_temp11);
      return Seq_Enumerator(_temp10);
    }));
  });
  Seq_Enumerator = (function (xs)
  {
    return xs.GetEnumerator();
  });
  Seq_Fold = (function (f, seed, xs)
  {
    return Seq_FoldIndexed((function (_arg1)
    {
      return (function (acc)
      {
        return (function (x)
        {
          return f(acc)(x);
        });
      });
    }), seed, xs);
  });
  Seq_FoldIndexed = (function (f, seed, xs)
  {
    return Seq_FoldIndexedAux(f, 0.000000, seed, Seq_Enumerator(xs));
  });
  Seq_FoldIndexedAux = (function (f, i, acc, xs)
  {
    if (xs.MoveNext()) 
    {
      return Seq_FoldIndexedAux(f, (i + 1.000000), f(i)(acc)(xs.get_Current()), xs);
    }
    else
    {
      return acc;
    };
  });
  Seq_FromFactory = (function (f)
  {
    return (new i_CreateEnumerable_1__ctor(f));
  });
  Seq_GroupBy = (function (f, xs)
  {
    var _temp99;
    var _f = (function (tupledArg)
    {
      var k = tupledArg.Item1;
      var vs = tupledArg.Item2;
      return {CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: k, Item2: Seq_OfList(vs)};
    });
    _temp99 = (function (_xs)
    {
      return Seq_Map(_f, _xs);
    });
    return _temp99((function (table)
    {
      return Map_ToSeq(table);
    })(Seq_Fold((function (acc)
    {
      return (function (x)
      {
        var k = f(x);
        var matchValue = acc.TryFind(k);
        if ((matchValue.Tag == "None")) 
        {
          return acc.Add(k, List_CreateCons(x, List_Empty()));
        }
        else
        {
          var vs = matchValue.Value;
          return acc.Add(k, List_CreateCons(x, vs));
        };
      });
    }), Map_Empty(), xs)));
  });
  Seq_Iterate = (function (f, xs)
  {
    var _temp92;
    return Seq_Fold((function (unitVar0)
    {
      return (function (x)
      {
        return f(x);
      });
    }), _temp92, xs);
  });
  Seq_IterateIndexed = (function (f, xs)
  {
    var _temp24;
    return Seq_FoldIndexed((function (i)
    {
      return (function (unitVar1)
      {
        return (function (x)
        {
          return f(i)(x);
        });
      });
    }), _temp24, xs);
  });
  Seq_Length = (function (xs)
  {
    return Seq_Fold((function (count)
    {
      return (function (_arg1)
      {
        return (count + 1.000000);
      });
    }), 0.000000, xs);
  });
  Seq_Map = (function (f, xs)
  {
    return (function (_f)
    {
      return Seq_Delay(_f);
    })((function (unitVar0)
    {
      var _temp98;
      var _f = (function (_enum)
      {
        if (_enum.MoveNext()) 
        {
          return {Tag: "Some", Value: {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: f(_enum.get_Current()), Item2: _enum}};
        }
        else
        {
          return {Tag: "None"};
        };
      });
      _temp98 = (function (seed)
      {
        return Seq_Unfold(_f, seed);
      });
      return _temp98(Seq_Enumerator(xs));
    }));
  });
  Seq_Map2 = (function (f, xs, ys)
  {
    return (function (_f)
    {
      return Seq_Delay(_f);
    })((function (unitVar0)
    {
      var _xs = Seq_Enumerator(xs);
      var _ys = Seq_Enumerator(ys);
      var _temp15;
      var _f = (function (_unitVar0)
      {
        if ((_xs.MoveNext() && _ys.MoveNext())) 
        {
          var _temp17;
          var _temp18;
          _temp17 = {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: f(_xs.get_Current())(_ys.get_Current()), Item2: _temp18};
          return {Tag: "Some", Value: _temp17};
        }
        else
        {
          return {Tag: "None"};
        };
      });
      _temp15 = (function (seed)
      {
        return Seq_Unfold(_f, seed);
      });
      var _temp19;
      return _temp15(_temp19);
    }));
  });
  Seq_MapIndexed = (function (f, xs)
  {
    return (function (_f)
    {
      return Seq_Delay(_f);
    })((function (unitVar0)
    {
      var _temp141;
      var _f = (function (tupledArg)
      {
        var _enum = tupledArg.Item1;
        var i = tupledArg.Item2;
        if (_enum.MoveNext()) 
        {
          return {Tag: "Some", Value: {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: f(i)(_enum.get_Current()), Item2: {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: _enum, Item2: (i + 1.000000)}}};
        }
        else
        {
          return {Tag: "None"};
        };
      });
      _temp141 = (function (seed)
      {
        return Seq_Unfold(_f, seed);
      });
      return _temp141({CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: Seq_Enumerator(xs), Item2: 0.000000});
    }));
  });
  Seq_OfArray = (function (xs)
  {
    var _temp23;
    var f = (function (i)
    {
      if ((i.CompareTo(Array_BoxedLength(xs)) < 0.000000)) 
      {
        return {Tag: "Some", Value: {CompareTo: (function (that)
        {
          var diff;
          return 0.000000;
        }), Item1: xs[i], Item2: (i + 1.000000)}};
      }
      else
      {
        return {Tag: "None"};
      };
    });
    _temp23 = (function (seed)
    {
      return Seq_Unfold(f, seed);
    });
    return _temp23(0.000000);
  });
  Seq_OfList = (function (xs)
  {
    var _temp97;
    var f = (function (_arg1)
    {
      if ((_arg1.Tag == "Cons")) 
      {
        var _xs = List_Tail(_arg1);
        var x = List_Head(_arg1);
        return {Tag: "Some", Value: {CompareTo: (function (that)
        {
          var diff;
          return 0.000000;
        }), Item1: x, Item2: _xs}};
      }
      else
      {
        return {Tag: "None"};
      };
    });
    _temp97 = (function (seed)
    {
      return Seq_Unfold(f, seed);
    });
    return _temp97(xs);
  });
  Seq_Take = (function (n, xs)
  {
    return (function (f)
    {
      return Seq_Delay(f);
    })((function (unitVar0)
    {
      var _xs = Seq_Enumerator(xs);
      var _temp78;
      var f = (function (i)
      {
        if (((i.CompareTo(n) < 0.000000) && _xs.MoveNext())) 
        {
          return {Tag: "Some", Value: {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: _xs.get_Current(), Item2: (i + 1.000000)}};
        }
        else
        {
          return {Tag: "None"};
        };
      });
      _temp78 = (function (seed)
      {
        return Seq_Unfold(f, seed);
      });
      return _temp78(0.000000);
    }));
  });
  Seq_ToArray = (function (xs)
  {
    var ys = Array_ZeroCreate(Seq_Length(xs));
    var _temp25;
    var f = (function (i)
    {
      return (function (x)
      {
        ys[i] = x;
        return null;
      });
    });
    _temp25 = (function (_xs)
    {
      return Seq_IterateIndexed(f, _xs);
    });
    _temp25(xs);
    return ys;
  });
  Seq_TryFind = (function (f, xs)
  {
    return Seq_TryPickIndexed((function (_arg1)
    {
      return (function (x)
      {
        if (f(x)) 
        {
          return {Tag: "Some", Value: x};
        }
        else
        {
          return {Tag: "None"};
        };
      });
    }), xs);
  });
  Seq_TryPickIndexed = (function (f, xs)
  {
    return Seq_TryPickIndexedAux(f, 0.000000, Seq_Enumerator(xs));
  });
  Seq_TryPickIndexedAux = (function (f, i, xs)
  {
    if (xs.MoveNext()) 
    {
      var result = f(i)(xs.get_Current());
      if ((result.Tag == "None")) 
      {
        return Seq_TryPickIndexedAux(f, (i + 1.000000), xs);
      }
      else
      {
        return result;
      };
    }
    else
    {
      return {Tag: "None"};
    };
  });
  Seq_Unfold = (function (f, seed)
  {
    return Seq_FromFactory((function (unitVar0)
    {
      return (new i_UnfoldEnumerator_2__ctor(seed, f));
    }));
  });
  String_IndexOf = (function (s, search)
  {
    return s.indexOf(search);
  });
  String_IsNullOrEmpty = (function (s)
  {
    return (s==null)||(s=="");
  });
  String_Join = (function (separator, s)
  {
    return s.join(separator);
  });
  String_Replace = (function (s, search, replace)
  {
    var res = s;
    while ((String_IndexOf(res, search).CompareTo(-1.000000) > 0.000000))
    {
      res = String_replaceSingle(res, search, replace);
      null;
    };
    return res;
  });
  String_Split = (function (s, delimiters)
  {
    var _temp31;
    var folder = (function (inputs)
    {
      return (function (delimiter)
      {
        var _temp33;
        var _temp34;
        var mapping = (function (inp)
        {
          return String_splitSingle(inp, delimiter);
        });
        _temp34 = (function (array)
        {
          return Array_Map(mapping, array);
        });
        _temp33 = _temp34(inputs);
        return (function (arrays)
        {
          return Array_Concat(Seq_OfArray(arrays));
        })(_temp33);
      });
    });
    var state = [s];
    _temp31 = (function (array)
    {
      return Array_Fold(folder, state, array);
    });
    return _temp31(delimiters);
  });
  String_ToLowerCase = (function (s)
  {
    return s.toLowerCase();
  });
  String_replaceSingle = (function (s, search, replace)
  {
    return s.replace(search, replace);
  });
  String_splitSingle = (function (s, delimiter)
  {
    return s.split(delimiter);
  });
  i_ApiaryJsContext__ctor = (function (Root, GlobalQuery, GlobalHeaders, GlobalArguments)
  {
    this.Root = Root;
    this.GlobalQuery = GlobalQuery;
    this.GlobalHeaders = GlobalHeaders;
    this.GlobalArguments = GlobalArguments;
  });
  i_AsyncBuilder__ctor = (function (unitVar0)
  {
    this.Bind = (function (_arg1, f)
    {
      var x = this;
      var v = _arg1.Item;
      return (function (_f)
      {
        return Async_protectedCont(_f);
      })((function (k)
      {
        var cont = (function (a)
        {
          var patternInput = f(a);
          var r = patternInput.Item;
          return r(k);
        });
        return v((new i_AsyncParams_1__ctor(cont, k.Aux)));
      }));
    });
    this.Delay = (function (f)
    {
      var x = this;
      return (function (_f)
      {
        return Async_protectedCont(_f);
      })((function (k)
      {
        var _temp56;
        var _temp57;
        _temp56 = f(_temp57);
        var patternInput = _temp56;
        var r = patternInput.Item;
        return r(k);
      }));
    });
    this.Zero = (function (unitVar1)
    {
      var x = this;
      return (function (f)
      {
        return Async_protectedCont(f);
      })((function (k)
      {
        var _temp58;
        return Async_invokeCont(k, _temp58);
      }));
    });
    this.ReturnFrom = (function (w)
    {
      var x = this;
      return w;
    });
    this.Return = (function (v)
    {
      var x = this;
      return (function (f)
      {
        return Async_protectedCont(f);
      })((function (k)
      {
        return Async_invokeCont(k, v);
      }));
    });
    this.While = (function (cond, body)
    {
      var __ = this;
      var x = __;
      var loop;
      loop = (function (_unitVar0)
      {
        var _temp61;
        var _temp62;
        _temp61 = cond(_temp62);
        if (_temp61) 
        {
          return x.Bind(body, loop);
        }
        else
        {
          return x.Zero();
        };
      });
      var _temp59;
      return loop(_temp59);
    });
    this.Combine = (function (work1, work2)
    {
      var __ = this;
      return __.Bind(work1, (function (_unitVar0)
      {
        return work2;
      }));
    });
    this.For = (function (seq, body)
    {
      var __ = this;
      var en = seq.GetEnumerator();
      var x = __;
      var loop;
      loop = (function (_unitVar0)
      {
        if (en.MoveNext()) 
        {
          return x.Bind(body(en.get_Current()), loop);
        }
        else
        {
          return x.Zero();
        };
      });
      var _temp63;
      return loop(_temp63);
    });
  });
  i_AsyncParamsAux__ctor = (function (ExceptionCont, CancelledCont, CancellationToken)
  {
    this.ExceptionCont = ExceptionCont;
    this.CancelledCont = CancelledCont;
    this.CancellationToken = CancellationToken;
  });
  i_AsyncParams_1__ctor = (function (Cont, Aux)
  {
    this.Cont = Cont;
    this.Aux = Aux;
  });
  i_CancellationToken__ctor = (function (Cell)
  {
    this.Cell = Cell;
    this.ThrowIfCancellationRequested = (function (unitVar1)
    {
      var x = this;
      var matchValue = x.Cell;
      if ((matchValue.Tag == "Some")) 
      {
        var _temp1;
        var cell = matchValue.Value;
        _temp1 = cell.contents;
        if (_temp1) 
        {
          var _cell = matchValue.Value;
          throw ("OperationCancelledException");
          return null;
        }
        else
        {
          ;
        };
      }
      else
      {
        ;
      };
    });
  });
  i_CreateEnumerable_1__ctor = (function (factory)
  {
    this.factory = factory;
    this.CompareTo = (function (ys)
    {
      var __ = this;
      var xs = __;
      return Seq_CompareWith((function (x)
      {
        return (function (y)
        {
          return LanguagePrimitives_UnboxGeneric(x).CompareTo(y);
        });
      }), xs, ys);
    });
    this.GetEnumerator = (function (unitVar1)
    {
      var __ = this;
      var _temp22;
      return __.factory(_temp22);
    });
  });
  i_FSharpMap_2__ctor = (function (comparer, tree)
  {
    this.comparer_386 = comparer;
    this.tree_390 = tree;
    this.get_Comparer = (function (unitVar1)
    {
      var m = this;
      return m.comparer_386;
    });
    this.get_Tree = (function (unitVar1)
    {
      var m = this;
      return m.tree_390;
    });
    this.Add = (function (k, v)
    {
      var m = this;
      return (new i_FSharpMap_2__ctor(m.comparer_386, MapTreeModule_add(m.comparer_386, k, v, m.tree_390)));
    });
    this.get_IsEmpty = (function (unitVar1)
    {
      var m = this;
      return MapTreeModule_isEmpty(m.tree_390);
    });
    this.get_Item = (function (k)
    {
      var m = this;
      return MapTreeModule_find(m.comparer_386, k, m.tree_390);
    });
    this.TryPick = (function (f)
    {
      var m = this;
      return MapTreeModule_tryPick(f, m.tree_390);
    });
    this.Exists = (function (f)
    {
      var m = this;
      return MapTreeModule_exists(f, m.tree_390);
    });
    this.Filter = (function (f)
    {
      var m = this;
      return (new i_FSharpMap_2__ctor(m.comparer_386, MapTreeModule_filter(m.comparer_386, f, m.tree_390)));
    });
    this.ForAll = (function (f)
    {
      var m = this;
      return MapTreeModule_forall(f, m.tree_390);
    });
    this.Fold = (function (f, acc)
    {
      var m = this;
      var _f = f;
      return MapTreeModule_foldBack(_f, m.tree_390, acc);
    });
    this.FoldSection = (function (lo, hi, f, acc)
    {
      var m = this;
      return MapTreeModule_foldSection(m.comparer_386, lo, hi, f, m.tree_390, acc);
    });
    this.Iterate = (function (f)
    {
      var m = this;
      return MapTreeModule_iter(f, m.tree_390);
    });
    this.MapRange = (function (f)
    {
      var m = this;
      return (new i_FSharpMap_2__ctor(m.comparer_386, MapTreeModule_map(f, m.tree_390)));
    });
    this.Map = (function (f)
    {
      var m = this;
      return (new i_FSharpMap_2__ctor(m.comparer_386, MapTreeModule_mapi(f, m.tree_390)));
    });
    this.Partition = (function (f)
    {
      var m = this;
      var patternInput = MapTreeModule_partition(m.comparer_386, f, m.tree_390);
      var r2 = patternInput.Item2;
      var r1 = patternInput.Item1;
      return {CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: (new i_FSharpMap_2__ctor(m.comparer_386, r1)), Item2: (new i_FSharpMap_2__ctor(m.comparer_386, r2))};
    });
    this.get_Count = (function (unitVar1)
    {
      var m = this;
      return MapTreeModule_size(m.tree_390);
    });
    this.ContainsKey = (function (k)
    {
      var m = this;
      return MapTreeModule_mem(m.comparer_386, k, m.tree_390);
    });
    this.Remove = (function (k)
    {
      var m = this;
      return (new i_FSharpMap_2__ctor(m.comparer_386, MapTreeModule_remove(m.comparer_386, k, m.tree_390)));
    });
    this.TryFind = (function (k)
    {
      var m = this;
      return MapTreeModule_tryFind(m.comparer_386, k, m.tree_390);
    });
    this.ToList = (function (unitVar1)
    {
      var m = this;
      return MapTreeModule_toList(m.tree_390);
    });
    this.ToArray = (function (unitVar1)
    {
      var m = this;
      return MapTreeModule_toArray(m.tree_390);
    });
    this.Equals = (function (that)
    {
      var __ = this;
      return false;
    });
    this.GetHashCode = (function (unitVar1)
    {
      var __ = this;
      return 0.000000;
    });
    this.GetEnumerator = (function (unitVar1)
    {
      var m = this;
      return MapTreeModule_mkIEnumerator(m.tree_390);
    });
    this.CompareTo = (function (obj)
    {
      var m = this;
      var m2 = LanguagePrimitives_UnboxGeneric(obj);
      return Seq_CompareWith((function (kvp1)
      {
        return (function (kvp2)
        {
          var c = m.comparer_386.Compare(kvp1.get_Key(), kvp2.get_Key());
          if ((c.CompareTo(0.000000) != 0.000000)) 
          {
            return c;
          }
          else
          {
            return kvp1.get_Value().CompareTo(kvp2.get_Value());
          };
        });
      }), m, m2);
    });
  });
  i_GenericComparer_1__ctor = (function (unitVar0)
  {
    this.Compare = (function (x, y)
    {
      var __ = this;
      return x.CompareTo(y);
    });
  });
  i_KeyValuePair_2__ctor = (function (key, value)
  {
    this.key = key;
    this.value = value;
    this.get_Key = (function (unitVar1)
    {
      var __ = this;
      return __.key;
    });
    this.get_Value = (function (unitVar1)
    {
      var __ = this;
      return __.value;
    });
  });
  i_MapIterator_2__ctor = (function (stack, started)
  {
    this.stack = stack;
    this.started = started;
  });
  i_OperationArguments__ctor = (function (Method, Path, Arguments, Headers, Query)
  {
    this.Method = Method;
    this.Path = Path;
    this.Arguments = Arguments;
    this.Headers = Headers;
    this.Query = Query;
    this.CompareTo = (function (that)
    {
      var diff;
      diff = this.Method.CompareTo(that.Method);
      if ((diff != 0.000000)) 
      {
        return diff;
      }
      else
      {
        diff = this.Path.CompareTo(that.Path);
        if ((diff != 0.000000)) 
        {
          return diff;
        }
        else
        {
          diff = this.Arguments.CompareTo(that.Arguments);
          if ((diff != 0.000000)) 
          {
            return diff;
          }
          else
          {
            diff = this.Headers.CompareTo(that.Headers);
            if ((diff != 0.000000)) 
            {
              return diff;
            }
            else
            {
              diff = this.Query.CompareTo(that.Query);
              if ((diff != 0.000000)) 
              {
                return diff;
              }
              else
              {
                return 0.000000;
              };
            };
          };
        };
      };
    });
  });
  i_UnfoldEnumerator_2__ctor = (function (seed, unfold)
  {
    this.seed = seed;
    this.unfold = unfold;
    this.acc = {Tag: "Some", Value: this.seed};
    this.current = null;
    this.Reset = (function (unitVar1)
    {
      var __ = this;
      __.acc = {Tag: "Some", Value: __.seed};
      __.current = null;
    });
    this.get_Current = (function (unitVar1)
    {
      var __ = this;
      return __.current;
    });
    this.MoveNext = (function (unitVar1)
    {
      var __ = this;
      var matchValue = __.acc;
      var _temp7;
      var currAcc = matchValue.Value;
      var _matchValue = __.unfold(currAcc);
      if ((_matchValue.Tag == "Some")) 
      {
        var value = _matchValue.Value.Item1;
        var nextAcc = _matchValue.Value.Item2;
        __.acc = {Tag: "Some", Value: nextAcc};
        __.current = value;
        _temp7 = true;
      }
      else
      {
        __.acc = {Tag: "None"};
        __.current = null;
        _temp7 = false;
      };
      return ((matchValue.Tag == "Some") && _temp7);
    });
    this.Dispose = (function (unitVar1)
    {
      var __ = this;
    });
  });
  i_mkIEnumerator__2__ctor = (function (s)
  {
    this.s = s;
    this.i = {contents: MapTreeModule_mkIterator(this.s)};
    this.get_Current = (function (unitVar1)
    {
      var self = this;
      return MapTreeModule_current(self.i.contents);
    });
    this.MoveNext = (function (unitVar1)
    {
      var self = this;
      return MapTreeModule_moveNext(self.i.contents);
    });
    this.Reset = (function (unitVar1)
    {
      var self = this;
      self.i.contents = MapTreeModule_mkIterator(self.s);
    });
    this.Dispose = (function (unitVar1)
    {
      var self = this;
    });
  });
  list_1_Cons = (function (Item1, Item2)
  {
    this.Tag = "Cons";
    this.Item1 = Item1;
    this.Item2 = Item2;
    this.CompareTo = (function (that)
    {
      var diff;
      return 0.000000;
    });
  });
  list_1_Nil = (function ()
  {
    this.Tag = "Nil";
    this.CompareTo = (function (that)
    {
      var diff;
      return 0.000000;
    });
  });
  return Program_main();
}
})()