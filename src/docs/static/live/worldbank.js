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
  var list_1_Nil, list_1_Cons, i_WorldBankCountry__ctor, i_UnfoldEnumerator_2__ctor, i_CreateEnumerable_1__ctor, i_CancellationToken__ctor, i_AsyncParams_1__ctor, i_AsyncParamsAux__ctor, i_AsyncBuilder__ctor, WorldBankRuntime_GetIndicators, WorldBankRuntime_GetCountry, WorldBankRuntime_GetCountries, WorldBankRuntime_AsyncGetIndicator, String_Join, Seq_Unfold, Seq_TryPickIndexedAux, Seq_TryPickIndexed, Seq_TryFind, Seq_ToArray, Seq_OfList, Seq_OfArray, Seq_Map2, Seq_Map, Seq_Length, Seq_IterateIndexed, Seq_FromFactory, Seq_FoldIndexedAux, Seq_FoldIndexed, Seq_Fold, Seq_Enumerator, Seq_Delay, Seq_CompareWith, Seq_Choose, Runtime_worldBankUrl, Program_test, Program_op_Dynamic, Program_number, Program_main, Program_get_data, Program_floor, Program_createChecks, Program_countries, List_ToArray, List_Tail, List_Reverse, List_OfArray, List_MapIndexed, List_Map, List_Length, List_IterateIndexed, List_Head, List_FoldIndexedAux, List_FoldIndexed, List_Fold, List_Empty, List_CreateCons, LanguagePrimitives_UnboxGeneric, JsonParser_parse, JsonOperations_TryConvertProperty, JsonOperations_GetText, JsonOperations_GetSingleByTypeTag, JsonOperations_GetProperty, JsonOperations_ConvertArray, JsonDocument_Create, FSharpString_Concat, Async_protectedCont, Async_invokeCont, Async_get_async, Async_StartImmediate, Async_FromContinuations, Async_1_Cont, Array_ZeroCreate, Array_Length, Array_FoldBackIndexed, Array_FoldBack, Array_BoxedLength, ApiaryCompiler_getJSON, ApiaryCompiler_encodeURIComponent;
  ApiaryCompiler_encodeURIComponent = (function (s)
  {
    return encodeURIComponent(s);
  });
  ApiaryCompiler_getJSON = (function (url, callback)
  {
    return $.getJSON(url, callback);
  });
  Array_BoxedLength = (function (xs)
  {
    return xs.length;
  });
  Array_FoldBack = (function (f, xs, seed)
  {
    return Array_FoldBackIndexed((function (_arg1)
    {
      return (function (x)
      {
        return (function (acc)
        {
          return f(x)(acc);
        });
      });
    }), xs, seed);
  });
  Array_FoldBackIndexed = (function (f, xs, seed)
  {
    var acc = seed;
    var size = Array_Length(xs);
    for (var _temp2 = 1.000000; _temp2 <= size; _temp2++)
    {
      (function (i)
      {
        acc = f((i - 1.000000))(xs[(size - i)])(acc);
        null;
      })(_temp2);
    };
    return acc;
  });
  Array_Length = (function (xs)
  {
    return xs.length;
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
    var _temp115;
    if ((cancellationToken.Tag == "Some")) 
    {
      var v = cancellationToken.Value;
      _temp115 = v;
    }
    else
    {
      _temp115 = (new i_CancellationToken__ctor({Tag: "None"}));
    };
    var token = _temp115;
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
  JsonOperations_GetProperty = (function (doc, name)
  {
    return doc[name];
  });
  JsonOperations_GetSingleByTypeTag = (function (doc, tag)
  {
    
    var doc = doc;
    var tag = tag;
    var res = [];
    for(i = 0; i<doc.length; i++) {
      var el = doc[i];
      if ( (tag == 1 && typeof(el) == "number") ||
           (tag == 2 && typeof(el) == "boolean") ||
           (tag == 3 && (typeof(el) == "object" && !Array.isArray(el))) ||
           (tag == 4 && Array.isArray(el)) ||
           (tag == 5 && typeof(el) == "string") )
      res.push(el);
    }
    if (res.length != 1) throw "JSON mismatch";
    return res[0];
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
  List_IterateIndexed = (function (f, xs)
  {
    var _temp104;
    return List_FoldIndexed((function (i)
    {
      return (function (unitVar1)
      {
        return (function (x)
        {
          return f(i)(x);
        });
      });
    }), _temp104, xs);
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
  List_Map = (function (f, xs)
  {
    return (function (_xs)
    {
      return List_Reverse(_xs);
    })(List_Fold((function (acc)
    {
      return (function (x)
      {
        return (new list_1_Cons(f(x), acc));
      });
    }), (new list_1_Nil()), xs));
  });
  List_MapIndexed = (function (f, xs)
  {
    return (function (_xs)
    {
      return List_Reverse(_xs);
    })(List_FoldIndexed((function (i)
    {
      return (function (acc)
      {
        return (function (x)
        {
          return (new list_1_Cons(f(i)(x), acc));
        });
      });
    }), (new list_1_Nil()), xs));
  });
  List_OfArray = (function (xs)
  {
    return Array_FoldBack((function (x)
    {
      return (function (acc)
      {
        return (new list_1_Cons(x, acc));
      });
    }), xs, (new list_1_Nil()));
  });
  List_Reverse = (function (xs)
  {
    return List_Fold((function (acc)
    {
      return (function (x)
      {
        return (new list_1_Cons(x, acc));
      });
    }), (new list_1_Nil()), xs);
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
  Program_countries = (function (unitVar0)
  {
    var _temp3;
    var var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp3 = WorldBankRuntime_GetCountry(var0, "ARB", "Arab World");
    var _temp4;
    var _var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp4 = WorldBankRuntime_GetCountry(_var0, "EUU", "European Union");
    var _temp5;
    var __var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp5 = WorldBankRuntime_GetCountry(__var0, "AUS", "Australia");
    var _temp6;
    var ___var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp6 = WorldBankRuntime_GetCountry(___var0, "BRA", "Brazil");
    var _temp7;
    var ____var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp7 = WorldBankRuntime_GetCountry(____var0, "CAN", "Canada");
    var _temp8;
    var _____var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp8 = WorldBankRuntime_GetCountry(_____var0, "CHL", "Chile");
    var _temp9;
    var ______var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp9 = WorldBankRuntime_GetCountry(______var0, "CZE", "Czech Republic");
    var _temp10;
    var _______var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp10 = WorldBankRuntime_GetCountry(_______var0, "DNK", "Denmark");
    var _temp11;
    var ________var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp11 = WorldBankRuntime_GetCountry(________var0, "FRA", "France");
    var _temp12;
    var _________var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp12 = WorldBankRuntime_GetCountry(_________var0, "GRC", "Greece");
    var _temp13;
    var __________var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp13 = WorldBankRuntime_GetCountry(__________var0, "LIC", "Low income");
    var _temp14;
    var ___________var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp14 = WorldBankRuntime_GetCountry(___________var0, "HIC", "High income");
    var _temp15;
    var ____________var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp15 = WorldBankRuntime_GetCountry(____________var0, "GBR", "United Kingdom");
    var _temp16;
    var _____________var0 = WorldBankRuntime_GetCountries(Program_get_data());
    _temp16 = WorldBankRuntime_GetCountry(_____________var0, "USA", "United States");
    return [_temp3, _temp4, _temp5, _temp6, _temp7, _temp8, _temp9, _temp10, _temp11, _temp12, _temp13, _temp14, _temp15, _temp16];
  });
  Program_createChecks = (function (unitVar0)
  {
    var _temp1;
    var mapping = (function (index)
    {
      return (function (country)
      {
        var input = jQuery("\u003cinput\u003e").attr("type", "checkbox");
        var label = jQuery("\u003clabel\u003e").append([input, country.Name]);
        var panel = (Program_floor((index % 3.000000)) + 1.000000);
        (function (value)
        {
          ;
        })(label.appendTo(jQuery(("#countryList" + panel.toString()))));
        return {CompareTo: (function (that)
        {
          var diff;
          return 0.000000;
        }), Item1: country, Item2: input};
      });
    });
    _temp1 = (function (list)
    {
      return List_MapIndexed(mapping, list);
    });
    return _temp1((function (array)
    {
      return List_OfArray(array);
    })(Program_countries()));
  });
  Program_floor = (function (n)
  {
    return Math.floor(n);
  });
  Program_get_data = (function ()
  {
    return {ServiceUrl: "http://api.worldbank.org", Source: "World Development Indicators"};
  });
  Program_main = (function (unitVar0)
  {
    return Program_op_Dynamic(jQuery, "document").ready((function (_unitVar0)
    {
      return Program_test();
    }));
  });
  Program_number = (function (a)
  {
    return a*1.0;
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
  Program_test = (function (unitVar0)
  {
    var infos = Program_createChecks();
    var render = (function (_unitVar0)
    {
      return (function (builder_)
      {
        return builder_.Delay((function (unitVar)
        {
          var series = {contents: List_Empty()};
          return builder_.Combine(builder_.For(Seq_OfList(infos), (function (_arg1)
          {
            var country = _arg1.Item1;
            var check = _arg1.Item2;
            if (check.is(":checked")) 
            {
              var _temp90;
              var var0 = WorldBankRuntime_GetIndicators(country);
              _temp90 = WorldBankRuntime_AsyncGetIndicator(var0, "SE.TER.ENRR");
              return builder_.Bind(_temp90, (function (_arg2)
              {
                var vals = _arg2;
                var _temp95;
                var _temp97;
                var _temp98;
                var mapping = (function (tupledArg)
                {
                  var k = tupledArg.Item1;
                  var v = tupledArg.Item2;
                  return [Program_number(k), Program_number(v)];
                });
                _temp98 = (function (source)
                {
                  return Seq_Map(mapping, source);
                });
                _temp97 = _temp98(vals);
                _temp95 = (function (source)
                {
                  return Seq_ToArray(source);
                })(_temp97);
                var data = _temp95;
                var _temp99;
                var returnVal = {};
                returnVal.data = data;
                null;
                returnVal.name = country.Name;
                null;
                _temp99 = returnVal;
                var s = _temp99;
                series.contents = List_CreateCons(s, series.contents);
                null;
                return builder_.Zero();
              }));
            }
            else
            {
              return builder_.Zero();
            };
          })), builder_.Delay((function (_unitVar)
          {
            var opts = {};
            var _temp101;
            var returnVal = {};
            returnVal.renderTo = "chart";
            null;
            returnVal.type = "line";
            null;
            _temp101 = returnVal;
            opts.chart = _temp101;
            null;
            var _temp103;
            var _returnVal = {};
            _returnVal.text = "School enrollment, tertiary (% gross)";
            null;
            _temp103 = _returnVal;
            opts.title = _temp103;
            null;
            opts.series = (function (list)
            {
              return List_ToArray(list);
            })(series.contents);
            null;
            (function (value)
            {
              ;
            })((new Highcharts.Chart(opts)));
            return builder_.Zero();
          })));
        }));
      })(Async_get_async());
    });
    var enumerator = Seq_OfList(infos).GetEnumerator();
    try
    {
      while (enumerator.MoveNext())
      {
        var forLoopVar = enumerator.get_Current();
        var check = forLoopVar.Item2;
        (function (value)
        {
          ;
        })(check.click((function (_unitVar0)
        {
          var _temp117;
          var _temp118;
          _temp117 = render(_temp118);
          return (function (arg00)
          {
            return Async_StartImmediate(arg00, {Tag: "None"});
          })(_temp117);
        })));
      };
    }
    finally{
      if (false) 
      {
        LanguagePrimitives_UnboxGeneric(enumerator).Dispose();
      }
      else
      {
        ;
      };
    };
  });
  Runtime_worldBankUrl = (function (wb, functions, props)
  {
    var _temp55;
    var _temp65;
    var _temp70;
    var _temp71;
    var sep = "";
    _temp71 = (function (strings)
    {
      return FSharpString_Concat(sep, Seq_OfList(strings));
    });
    var _temp73;
    var _temp74;
    var mapping = (function (m)
    {
      return ("/" + ApiaryCompiler_encodeURIComponent(m));
    });
    _temp74 = (function (list)
    {
      return List_Map(mapping, list);
    });
    _temp73 = _temp74(functions);
    _temp70 = _temp71(_temp73);
    _temp65 = ((wb.ServiceUrl + "/") + _temp70);
    _temp55 = (_temp65 + "?per_page=1000");
    var _temp79;
    var _temp80;
    var _sep = "";
    _temp80 = (function (strings)
    {
      return FSharpString_Concat(_sep, Seq_OfList(strings));
    });
    var _temp82;
    var _temp83;
    var _mapping = (function (tupledArg)
    {
      var key = tupledArg.Item1;
      var value = tupledArg.Item2;
      return ((("\u0026" + key) + "=") + ApiaryCompiler_encodeURIComponent(value));
    });
    _temp83 = (function (list)
    {
      return List_Map(_mapping, list);
    });
    _temp82 = _temp83(props);
    _temp79 = _temp80(_temp82);
    return (_temp55 + _temp79);
  });
  Seq_Choose = (function (f, xs)
  {
    var trySkipToNext;
    trySkipToNext = (function (_enum)
    {
      if (_enum.MoveNext()) 
      {
        var matchValue = f(_enum.get_Current());
        if ((matchValue.Tag == "Some")) 
        {
          var value = matchValue.Value;
          return {Tag: "Some", Value: {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: value, Item2: _enum}};
        }
        else
        {
          return trySkipToNext(_enum);
        };
      }
      else
      {
        return {Tag: "None"};
      };
    });
    return (function (_f)
    {
      return Seq_Delay(_f);
    })((function (unitVar0)
    {
      var _temp85;
      var _f = trySkipToNext;
      _temp85 = (function (seed)
      {
        return Seq_Unfold(_f, seed);
      });
      return _temp85(Seq_Enumerator(xs));
    }));
  });
  Seq_CompareWith = (function (f, xs, ys)
  {
    var _temp30;
    var _temp31;
    var _f = (function (i)
    {
      return (i.CompareTo(0.000000) != 0.000000);
    });
    _temp31 = (function (_xs)
    {
      return Seq_TryFind(_f, _xs);
    });
    _temp30 = _temp31(Seq_Map2((function (x)
    {
      return (function (y)
      {
        return f(x)(y);
      });
    }), xs, ys));
    var nonZero = _temp30;
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
      var _temp20;
      var _temp21;
      _temp20 = f(_temp21);
      return Seq_Enumerator(_temp20);
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
  Seq_IterateIndexed = (function (f, xs)
  {
    var _temp34;
    return Seq_FoldIndexed((function (i)
    {
      return (function (unitVar1)
      {
        return (function (x)
        {
          return f(i)(x);
        });
      });
    }), _temp34, xs);
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
      var _temp91;
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
      _temp91 = (function (seed)
      {
        return Seq_Unfold(_f, seed);
      });
      return _temp91(Seq_Enumerator(xs));
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
      var _temp25;
      var _f = (function (_unitVar0)
      {
        if ((_xs.MoveNext() && _ys.MoveNext())) 
        {
          var _temp27;
          var _temp28;
          _temp27 = {CompareTo: (function (that)
          {
            var diff;
            return 0.000000;
          }), Item1: f(_xs.get_Current())(_ys.get_Current()), Item2: _temp28};
          return {Tag: "Some", Value: _temp27};
        }
        else
        {
          return {Tag: "None"};
        };
      });
      _temp25 = (function (seed)
      {
        return Seq_Unfold(_f, seed);
      });
      var _temp29;
      return _temp25(_temp29);
    }));
  });
  Seq_OfArray = (function (xs)
  {
    var _temp87;
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
    _temp87 = (function (seed)
    {
      return Seq_Unfold(f, seed);
    });
    return _temp87(0.000000);
  });
  Seq_OfList = (function (xs)
  {
    var _temp33;
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
    _temp33 = (function (seed)
    {
      return Seq_Unfold(f, seed);
    });
    return _temp33(xs);
  });
  Seq_ToArray = (function (xs)
  {
    var ys = Array_ZeroCreate(Seq_Length(xs));
    var _temp35;
    var f = (function (i)
    {
      return (function (x)
      {
        ys[i] = x;
        return null;
      });
    });
    _temp35 = (function (_xs)
    {
      return Seq_IterateIndexed(f, _xs);
    });
    _temp35(xs);
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
  String_Join = (function (separator, s)
  {
    return s.join(separator);
  });
  WorldBankRuntime_AsyncGetIndicator = (function (country, indicator)
  {
    return Async_FromContinuations((function (tupledArg)
    {
      var cont = tupledArg.Item1;
      var econt = tupledArg.Item2;
      var ccont = tupledArg.Item3;
      var wb = country.Context;
      var countryCode = country.Code;
      var url = Runtime_worldBankUrl(wb, List_CreateCons("countries", List_CreateCons(countryCode, List_CreateCons("indicators", List_CreateCons(indicator, List_Empty())))), List_CreateCons({CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: "date", Item2: "1900:2050"}, List_CreateCons({CompareTo: (function (that)
      {
        var diff;
        return 0.000000;
      }), Item1: "format", Item2: "jsonp"}, List_Empty())));
      return ApiaryCompiler_getJSON((url + "\u0026prefix=?"), (function (json)
      {
        var _temp84;
        var var0 = json;
        _temp84 = JsonDocument_Create(JsonParser_parse(var0));
        var data = _temp84;
        var _temp88;
        var _temp89;
        var chooser = (function (v)
        {
          var matchValue = JsonOperations_TryConvertProperty(v, "value", (function (x)
          {
            return JsonOperations_GetText(x);
          }));
          if ((matchValue.Tag == "Some")) 
          {
            var value = matchValue.Value;
            return {Tag: "Some", Value: {CompareTo: (function (that)
            {
              var diff;
              return 0.000000;
            }), Item1: JsonOperations_GetText(JsonOperations_GetProperty(v, "date")), Item2: value}};
          }
          else
          {
            return {Tag: "None"};
          };
        });
        _temp89 = (function (source)
        {
          return Seq_Choose(chooser, source);
        });
        _temp88 = _temp89((function (source)
        {
          return Seq_OfArray(source);
        })(JsonOperations_ConvertArray(JsonOperations_GetSingleByTypeTag(data, 4.000000), (function (x)
        {
          return x;
        }))));
        var res = _temp88;
        return cont(res);
      }));
    }));
  });
  WorldBankRuntime_GetCountries = (function (data)
  {
    return data;
  });
  WorldBankRuntime_GetCountry = (function (countries, code, name)
  {
    return (new i_WorldBankCountry__ctor(countries, code, name));
  });
  WorldBankRuntime_GetIndicators = (function (country)
  {
    return country;
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
        var _temp106;
        var _temp107;
        _temp106 = f(_temp107);
        var patternInput = _temp106;
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
        var _temp108;
        return Async_invokeCont(k, _temp108);
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
        var _temp111;
        var _temp112;
        _temp111 = cond(_temp112);
        if (_temp111) 
        {
          return x.Bind(body, loop);
        }
        else
        {
          return x.Zero();
        };
      });
      var _temp109;
      return loop(_temp109);
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
      var _temp113;
      return loop(_temp113);
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
        var _temp114;
        var cell = matchValue.Value;
        _temp114 = cell.contents;
        if (_temp114) 
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
      var _temp32;
      return __.factory(_temp32);
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
      var _temp17;
      var currAcc = matchValue.Value;
      var _matchValue = __.unfold(currAcc);
      if ((_matchValue.Tag == "Some")) 
      {
        var value = _matchValue.Value.Item1;
        var nextAcc = _matchValue.Value.Item2;
        __.acc = {Tag: "Some", Value: nextAcc};
        __.current = value;
        _temp17 = true;
      }
      else
      {
        __.acc = {Tag: "None"};
        __.current = null;
        _temp17 = false;
      };
      return ((matchValue.Tag == "Some") && _temp17);
    });
    this.Dispose = (function (unitVar1)
    {
      var __ = this;
    });
  });
  i_WorldBankCountry__ctor = (function (Context, Code, Name)
  {
    this.Context = Context;
    this.Code = Code;
    this.Name = Name;
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