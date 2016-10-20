% @hidden
-module(wok_req).
-include("../include/wok_req.hrl").
-export([
  '$handle_undefined_function'/2
  , set_response/2
  , get_response/1
  , set_response_code/2
  , get_response_code/1
  , set_response_headers/2
  , get_response_headers/1
  , set_response_body/2
  , get_response_body/1
  , get_http_req/1
  , set_http_req/2
  , get_custom_data/1
  , set_custom_data/2
  , get_global_state/1
  , set_global_state/2
  , get_local_state/1
  , set_local_state/2
  , get_handler/1
  , set_handler/2
  , get_post_values/1
  , set_post_values/2
  , get_get_values/1
  , set_get_values/2
  , get_bind_values/1
  , set_bind_values/2
  , get_files/1
  , set_files/2
  , get_tmp_dir/1
  , set_tmp_dir/2
  , new/2
  , terminate/1
]).
-export_type([wok_req/0]).

-type wok_req() :: #wok_req{}.

-callback reply(wok_req:wok_req()) -> term().
-callback set_cookie(wok_req:wok_req(),
                     iodata(),
                     iodata(),
                     [{max_age, non_neg_integer()}
                      | {domain, binary()}
                      | {path, binary()}
                      | {secure, boolean()}
                      | {http_only, boolean()}]) -> wok_req:wok_req().
-callback get_cookies(wok_req:wok_req()) -> [{binary(), binary()}].
-callback client_ip(wok_req:wok_req()) -> inet:ip_address().
-callback client_port(wok_req:wok_req()) -> inet:port_number().
-callback body(wok_req:wok_req()) -> {ok | more, binary(), wok_req:wok_req()}.
-callback has_body(wok_req:wok_req()) -> boolean().
-callback body_length(wok_req:wok_req()) -> integer().
-callback method(wok_req:wok_req()) -> term().
-callback path(wok_req:wok_req()) -> term().
-callback header(wok_req:wok_req(), binary(), any()) -> binary() | any() | undefined.
-callback headers(wok_req:wok_req()) -> [{binary(), iodata()}].
-callback post_values(term()) -> {ok,
                                  [{binary(), binary() | true}], % DATA
                                  [{binary(), binary(), binary()}], % FILES
                                  binary() | undefined,
                                  term()}
                                 | {error, term()}.
-callback get_values(term()) -> {ok, [{binary(), binary() | true}], term()}
                                | {error, term()}.
-callback binding_values(term()) -> {ok, [{binary(), binary() | true}], term()}
                                    | {error, term()}.

'$handle_undefined_function'(Fun, [#wok_req{adapter = Adapter}|_] = Args) ->
   erlang:apply(Adapter, Fun, Args).

set_response(Req, {Code, Headers, Body}) ->
  bucs:pipecall([
                 {fun set_response_code/2, [Req, Code]},
                 {fun set_response_headers/2, [Headers]},
                 {fun set_response_body/2, [Body]}
                ]).

get_response(#wok_req{response = Resp}) ->
  Resp.

set_response_code(#wok_req{response = Resp} = Req, Code) ->
  Req#wok_req{response = Resp#wok_resp{code = Code}}.

get_response_code(#wok_req{response = #wok_resp{code = Code}}) ->
  Code.

set_response_headers(#wok_req{response = Resp} = Req, Headers) ->
  Req#wok_req{response = Resp#wok_resp{headers = Headers}}.

get_response_headers(#wok_req{response = #wok_resp{headers = Headers}}) ->
  Headers.

set_response_body(#wok_req{response = Resp} = Req, Body) ->
  Req#wok_req{response = Resp#wok_resp{body = Body}}.

get_response_body(#wok_req{response = #wok_resp{body = Body}}) ->
  Body.

set_http_req(WokReq, HTTPReq) ->
  WokReq#wok_req{request = HTTPReq}.

get_http_req(#wok_req{request = HTTPReq}) ->
  HTTPReq.

get_custom_data(#wok_req{custom_data = CustomData}) ->
  CustomData.

set_custom_data(Req, CustomData) when is_map(CustomData) ->
  Req#wok_req{custom_data = CustomData}.

get_global_state(#wok_req{global_state = State}) ->
  State.

set_global_state(Req, State) ->
  Req#wok_req{global_state = State}.

get_local_state(#wok_req{local_state = State}) ->
  State.

set_local_state(Req, State) ->
  Req#wok_req{local_state = State}.

get_handler(#wok_req{handler = Handler}) ->
  Handler.

set_handler(Req, Handler) ->
  Req#wok_req{handler = Handler}.

get_post_values(#wok_req{params = #wok_req_params{post = List}}) ->
  List.

set_post_values(#wok_req{params = Params} = Req, List) when is_list(List) ->
  Req#wok_req{params = Params#wok_req_params{post = List}}.

get_get_values(#wok_req{params = #wok_req_params{get = List}}) ->
  List.

set_get_values(#wok_req{params = Params} = Req, List) when is_list(List) ->
  Req#wok_req{params = Params#wok_req_params{get = List}}.

get_bind_values(#wok_req{params = #wok_req_params{bind = List}}) ->
  List.

set_bind_values(#wok_req{params = Params} = Req, List) when is_list(List) ->
  Req#wok_req{params = Params#wok_req_params{bind = List}}.

get_files(#wok_req{params = #wok_req_params{files = List}}) ->
  List.

set_files(#wok_req{params = Params} = Req, List) when is_list(List) ->
  Req#wok_req{params = Params#wok_req_params{files = List}}.

get_tmp_dir(#wok_req{params = #wok_req_params{tmp_dir = Path}}) ->
  Path.

set_tmp_dir(#wok_req{params = Params} = Req, Path) ->
  Req#wok_req{params = Params#wok_req_params{tmp_dir = Path}}.

-spec new(Adapter :: atom(), Req :: term()) -> wok_req().
new(Adapter, Req) ->
  {GetParams1, Req1} = case erlang:apply(Adapter, get_values, [Req]) of
                         {ok, GetParams, Req0} ->
                           {GetParams, Req0};
                         {_, Req0} ->
                           {[], Req0}
                       end,
  {BindParams1, Req3} = case erlang:apply(Adapter, binding_values, [Req1]) of
                          {ok, BindParams, Req2} ->
                            {BindParams, Req2};
                          {_, Req2} ->
                            {[], Req2}
                        end,
  {PostParams1, Files1, TmpDir1, Req5} = case erlang:apply(Adapter, post_values, [Req3]) of
                                           {ok, PostParams, Files, TmpDir, Req4} ->
                                             {PostParams, Files, TmpDir, Req4};
                                           {_, Req4} ->
                                             {[], Req4}
                                         end,
  WokReq0 = set_http_req(#wok_req{adapter = Adapter}, Req5),
  WokReq1 = set_get_values(WokReq0, GetParams1),
  WokReq2 = set_bind_values(WokReq1, BindParams1),
  WokReq3 = set_post_values(WokReq2, PostParams1),
  WokReq4 = set_files(WokReq3, Files1),
  set_tmp_dir(WokReq4, TmpDir1).

terminate(#wok_req{params = #wok_req_params{tmp_dir = TmpDir}} = WokReq) ->
  case TmpDir of
    undefined -> WokReq;
    _ ->
      _ = bucfile:remove_recursive(TmpDir),
      set_tmp_dir(WokReq, undefined)
  end.

