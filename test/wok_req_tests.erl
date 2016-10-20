-module(wok_req_tests).
-include("../include/wok_req.hrl").
-include_lib("eunit/include/eunit.hrl").

wok_req_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
     fun() ->
        R = #wok_req{request = undefined,
                     custom_data = #{}},
        ?assertEqual(#{}, wok_req:get_custom_data(R)),
        R2 = wok_req:set_custom_data(R, #{hello => world}),
        ?assert(is_record(R2, wok_req)),
        ?assertEqual(#{hello => world}, wok_req:get_custom_data(R2))
     end
   ]}.
