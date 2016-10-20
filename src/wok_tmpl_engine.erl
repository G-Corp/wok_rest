% @hidden
-module(wok_tmpl_engine).

% API
-export([render/4, yield/3]).

% Compiler plugin
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).
-define(DEFAULT_COMPILER_OPTIONS, [
                                   {libraries, [{wok_helpers, wok_helpers_dtl}]},
                                   {default_libraries, [wok_helpers]},
                                   report_warnings,
                                   report_errors,
                                   warnings_as_errors
                                  ]).
-define(DEFAULT_SOURCE_OPTION, "templates").
-define(DEFAULT_EXT_OPTION, tmpl).
-define(DEFAULT_SUFFIX_OPTION, "_tmpl").
-define(DEFAULT_FULL_PATH_OPTION, true).
-define(DEFAULT_OPTIONS, [
                          {source, ?DEFAULT_SOURCE_OPTION},
                          {ext, ?DEFAULT_EXT_OPTION},
                          {suffix, ?DEFAULT_SUFFIX_OPTION},
                          {compiler_options, ?DEFAULT_COMPILER_OPTIONS},
                          {full_path, ?DEFAULT_FULL_PATH_OPTION}
                         ]).

% API
render(Code, Headers, Template, Data) when is_map(Headers) ->
  render(Code, bucmaps:to_list(Headers), Template, Data);
render(Code, Headers, Template, Data) when is_map(Data) ->
  render(Code, Headers, Template, bucmaps:to_list(Data));
render(Code, Headers, Template, Data) when is_list(Headers),
                                           is_list(Data) ->
  Headers1 = lists:map(fun({K, V}) ->
                           {<< <<(string:to_lower(C))>> || <<C>> <= (bucs:to_binary(K)) >>,
                            bucs:to_binary(V)}
                       end, Headers),
  Locale = buclists:keyfind(<<"content-language">>, 1, Headers1, <<>>),
  case yield(Template, Locale, Data) of
    {ok, Output} ->
      {Code, Headers1, iolist_to_binary(Output)};
    {error, Error} ->
      {500, [], bucs:to_binary(Error)}
  end.

template_module(Template) when is_atom(Template) ->
  code:ensure_loaded(Template);
template_module(Template) when is_list(Template);
                               is_binary(Template) ->
  template_module(
    bucs:pipecall([
                   {fun bucs:to_list/1, [Template]},
                   {fun re:replace/4, ["/", "_", [{return, list}, global]]},
                   {fun re:replace/4, ["\\.", "_", [{return, list}, global]]},
                   fun bucs:to_atom/1
                  ])).

yield(Template, Locale, Data) ->
  case template_module(Template) of
    {module, Template1} ->
      Data1 = lists:map(fun
                          ({K, _} = T) when is_atom(K) -> T;
                          ({K, V}) -> {bucs:to_atom(K), V}
                        end, Data),
      Data2 = [{'__all__', Data1}|Data1],
      erlang:apply(Template1, render, [Data2, [{translation_fun, fun wok_tmpl_i18n:translate/2},
                                               {locale, Locale}]]);
    _ ->
      {error, missing_template}
  end.

% Compiler plugin

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                             {module, ?MODULE},
                                                             {namespace, wok_tmpl_engine},
                                                             {bare, false},
                                                             {deps, ?DEPS},
                                                             {example, "rebar3 wok_tmpl_engine compile"},
                                                             {short_desc, "Compile erlydtl templates."},
                                                             {desc, "Compile erlydtl templates."},
                                                             {opts, []}])),
  {ok, State1}.

do(State) ->
  rebar_api:info("Running wok_tmpl_engine template compiler...", []),
  Apps = case rebar_state:current_app(State) of
           undefined ->
             rebar_state:project_apps(State);
           AppInfo ->
             [AppInfo]
         end,
  [begin
     Opts = rebar_app_info:opts(AppInfo),
     Dir = rebar_app_info:dir(AppInfo),
     OutDir = rebar_app_info:ebin_dir(AppInfo),
     ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),

     DtlOpts = proplists:unfold(rebar_opts:get(Opts, wok_tmpl_engine_options, ?DEFAULT_OPTIONS)),

     TemplateDir = <<(bucs:to_binary(
                        filename:join(
                          Dir,
                          proplists:get_value(source, DtlOpts, ?DEFAULT_SOURCE_OPTION))))/binary,
                     "/">>,
     SourceExtRe = "^[^._].*\\" ++
                   "." ++
                   atom_to_list(proplists:get_value(ext, DtlOpts, ?DEFAULT_EXT_OPTION)) ++
                   [$$],
     FoundFiles = rebar_utils:find_files(TemplateDir, SourceExtRe, true),
     [case compile_tmpl(bucs:to_binary(File), TemplateDir, bucs:to_binary(OutDir), DtlOpts) of
        error ->
          rebar_api:abort();
        _ ->
          ok
      end || File <- FoundFiles]
   end || AppInfo <- Apps],

  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

compile_tmpl(File, TemplateDir, OutDir, DtlOpts) ->
  TmplSuffix = bucs:to_binary(proplists:get_value(suffix, DtlOpts, ?DEFAULT_SUFFIX_OPTION)),
  Module0 = case proplists:get_value(full_path, DtlOpts, ?DEFAULT_FULL_PATH_OPTION) of
              true ->
                binary:replace(
                  binary:replace(File, TemplateDir, <<>>),
                  <<"/">>, <<"_">>, [global]);
              false ->
                filename:basename(File)
            end,
  Module1 = filename:rootname(Module0),
  Module2 = binary:replace(Module1, <<".">>, <<"_">>, [global]),
  Module3 = <<Module2/binary, TmplSuffix/binary>>,
  Module4 = bucs:to_atom(Module3),
  rebar_api:debug("Compile ~p to ~p in ~p", [File, Module4, OutDir]),
  case erlydtl:compile_file(
         bucs:to_string(File), Module4,
         [{out_dir, bucs:to_string(OutDir)}|
          proplists:get_value(compiler_options, DtlOpts, ?DEFAULT_COMPILER_OPTIONS)]) of
    {ok, _} -> {ok, Module1};
    {ok, _, _} -> {ok, Module1};
    {ok, _, _, _} -> {ok, Module1};
    _ -> error
  end.

