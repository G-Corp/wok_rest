% @hidden
-module(wok_rest_initializer).
-compile([{parse_transform, lager_transform}]).
-include("../include/wok_cowboy.hrl").

-export([start/0]).

start() ->
  case doteki:get_env([wok, rest]) of
    undefined ->
      lager:debug("No REST configuration."),
      #{};
    _ ->
      _ = application:ensure_all_started(cowboy),
      Port = doteki:get_env([wok, rest, port], ?DEFAULT_REST_PORT),
      IP = bucinet:to_ip(doteki:get_env([wok, rest, ip], ?DEFAULT_REST_IP)),
      TransOpts = [{port, Port}, {ip, IP}],
      MaxConn = doteki:get_env([wok, rest, max_conn], ?DEFAULT_REST_MAX_CONN),
      {Dispatch, Static} = wok_cowboy_handler:routes(),
      ProtoOpts   = [{env, [{dispatch, Dispatch}]},
                     {middlewares, [cowboy_router, cowboy_default_static_file, cowboy_handler]}],
      case cowboy:start_http(http, MaxConn, TransOpts, ProtoOpts) of
        {ok, _} ->
          lager:info("Start HTTP on port ~p", [Port]),
          Static;
        _ ->
          lager:error("Faild to start HTTP server"),
          exit(http_error)
      end
  end.
