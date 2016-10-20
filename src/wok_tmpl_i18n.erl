% @hidden
-module(wok_tmpl_i18n).

-export([lang/0, lang/1, translate/2]).

lang() ->
  case bucs:function_exists(wok_i18n, lang, 0) of
    true ->
      wok_i18n:lang();
    false ->
      undefined
  end.

lang(Lang) ->
  case bucs:function_exists(wok_i18n, lang, 1) of
    true ->
      wok_i18n:lang(Lang);
    false ->
      ok
  end.

translate(Data, Local) ->
  case bucs:function_exists(wok_i18n, translate, 2) of
    true ->
      wok_i18n:translate(Data, Local);
    false ->
      case Data of
        {Singular, {Plural, N}} ->
          case bucs:to_integer(N) > 1 of
            true -> Plural;
            false -> Singular
          end;
        _ ->
          Data
      end
  end.

