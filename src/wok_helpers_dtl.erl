% @hidden
-module(wok_helpers_dtl).
-behaviour(erlydtl_library).
-export([
         version/0
         , inventory/1
        ]).

inventory(filters) ->
  case bucs:function_exists(wok_helpers_lib, filters, 0) of
    true ->
      lists:map(fun(F) ->
                    {F, {wok_helpers_lib, F}}
                end, wok_helpers_lib:filters());
    _ ->
      []
  end;

inventory(tags) ->
  case bucs:function_exists(wok_helpers_lib, tags, 0) of
    true ->
      lists:map(fun(F) ->
                    {F, {wok_helpers_lib, F}}
                end, wok_helpers_lib:tags());
    _ ->
      []
  end.

version() -> 1.

