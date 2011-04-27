-module(quoted).

-export([to_list/1,
         to_binary/1,
         as_list/1,
         as_binary/1]).

%% internal exports
-on_load(load_nif/0).
-export([load_nif/0,
         '_nif_loaded'/0,
         '_nif_unquote'/2,
         '_erl_unquote'/2,
         '_nif_quote'/2,
         '_erl_quote'/2,
         test/0]).

load_nif() ->
    catch erlang:load_nif(nif_path(), 0),
    ok.

nif_path() ->
    So = "quoted",
    case code:priv_dir(quoted) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                File when not is_list(File) -> filename:join("../priv", So);
                File -> filename:join([filename:dirname(File),"../priv", So])
            end;
         Dir ->
            filename:join(Dir, So)
    end.

-spec '_nif_loaded'() -> boolean().
'_nif_loaded'() ->
    false.

%% Fall back to erlang implementation if the nif can't be loaded
-spec '_nif_unquote'(string() | binary(), list | binary) -> string() | binary().
'_nif_unquote'(String, Format) ->
    '_erl_unquote'(String, Format).


-spec '_erl_unquote'(string() | binary(), list | binary) -> string() | binary().
'_erl_unquote'(String, binary) when is_binary(String) ->
    unquote_bin(String);

'_erl_unquote'(String, binary) when is_list(String) ->
    list_to_binary(unquote_list(String));

'_erl_unquote'(String, list) when is_list(String) ->
    unquote_list(String);

'_erl_unquote'(String, list) when is_binary(String) ->
    binary_to_list(unquote_bin(String)).


%% Fall back to erlang implementation if the nif can't be loaded
-spec '_nif_quote'(string() | binary(), list | binary) -> string() | binary().
'_nif_quote'(String, Format) ->
    '_erl_quote'(String, Format).


-spec '_erl_quote'(string() | binary(), list | binary) -> string() | binary().
'_erl_quote'(String, binary) when is_binary(String) ->
    quote_bin(String);

'_erl_quote'(String, binary) when is_list(String) ->
    list_to_binary(quote_list(String));

'_erl_quote'(String, list) when is_list(String) ->
    quote_list(String);

'_erl_quote'(String, list) when is_binary(String) ->
    binary_to_list(quote_bin(String)).


-spec to_list(string() | binary()) -> string().
to_list(String) ->
    '_nif_unquote'(String, list).


-spec to_binary(string() | binary()) -> binary().
to_binary(String) ->
    '_nif_unquote'(String, binary).


-spec as_list(string() | binary()) -> string().
as_list(String) ->
    '_nif_quote'(String, list).


-spec as_binary(string() | binary()) -> binary().
as_binary(String) ->
    '_nif_quote'(String, binary).


test_decode(N, Name, Input) ->
    Bin = iolist_to_binary(Input),
    Str = binary_to_list(Bin),
    [Name,
    {test_type, nif_impl, erl_impl},
    {list_to_list,
        timeit(N, fun() -> quoted:to_list(Str) end),
        timeit(N, fun() -> quoted:'_erl_unquote'(Str, list) end)},
    {list_to_bin,
        timeit(N, fun() -> quoted:to_binary(Str) end),
        timeit(N, fun() -> quoted:'_erl_unquote'(Str, binary) end)},
    {bin_to_bin,
        timeit(N, fun() -> quoted:to_binary(Bin) end),
        timeit(N, fun() -> quoted:'_erl_unquote'(Bin, binary) end)},
    {bin_to_list,
        timeit(N, fun() -> quoted:to_list(Bin) end),
        timeit(N, fun() -> quoted:'_erl_unquote'(Bin, binary) end)}
    ].

test_encode(N, Name, Input) ->
    Bin = iolist_to_binary(Input),
    Str = binary_to_list(Bin),
    [Name,
    {test_type, nif_impl, erl_impl},
    {list_to_list,
        timeit(N, fun() -> quoted:as_list(Str) end),
        timeit(N, fun() -> quoted:'_erl_quote'(Str, list) end)},
    {list_to_bin,
        timeit(N, fun() -> quoted:as_binary(Str) end),
        timeit(N, fun() -> quoted:'_erl_quote'(Str, binary) end)},
    {bin_to_bin,
        timeit(N, fun() -> quoted:as_binary(Bin) end),
        timeit(N, fun() -> quoted:'_erl_quote'(Bin, binary) end)},
    {bin_to_list,
        timeit(N, fun() -> quoted:as_list(Bin) end),
        timeit(N, fun() -> quoted:'_erl_quote'(Bin, binary) end)}
    ].






test_32b(N) ->
    test_decode(N, 'Decode 32B unquoted string', ["a" || _ <- lists:seq(1, 32)]) ++
    test_encode(N, 'Encode 32B unquoted string', ["a" || _ <- lists:seq(1, 32)]).

test_1kb(N) ->
    test_decode(N, 'Decode 1KB unquoted string', ["a" || _ <- lists:seq(1, 1024)]) ++
    test_encode(N, 'Encode 1KB unquoted string', ["a" || _ <- lists:seq(1, 1024)]).

test_2kb(N) ->
    test_decode(N, 'Decode 2KB quoted string', ["%20" || _ <- lists:seq(1, 2048 div 3)]) ++
    test_encode(N, 'Encode 2KB quoted string', [" "   || _ <- lists:seq(1, 2048)]).

test() ->
    R = test_32b(10000)
    ++  test_1kb(10000)
    ++  test_2kb(10000),
    lists:foreach(fun(Res) ->
        io:format("~w~n", [Res])
    end, R).


timeit(Times, Fun) ->
    Tmp = lists:seq(1, Times),
    {Time, ok} = timer:tc(fun() ->
        [Fun() || _ <- Tmp],
        ok
    end, []),
    Time / Times.


%% Function for unquoting query string parameters borrowed from mochiweb.
%% slightly modified to not use an accumulator.


-spec unquote_list(string()) -> string().
unquote_list(String) when is_list(String) ->
    unquote_list_(String).


unquote_list_([]) ->
    [];

unquote_list_("%"   ++ [H,L|T]) ->
    is_hex(H) andalso is_hex(L) orelse erlang:error(badarg),
    Char = unhex(L) bor (unhex(H) bsl 4),
    [Char|unquote_list_(T)];

unquote_list_([Char|T]) ->
    [Char|unquote_list_(T)].

-spec unquote_bin(binary()) -> binary().
unquote_bin(String) when is_binary(String) ->
    unquote_bin_(String, <<>>).


unquote_bin_(<<>>, Acc) ->
    Acc;

unquote_bin_(<<"%", H, L, Rest/binary>>, Acc) ->
    is_hex(H) andalso is_hex(L) orelse erlang:error(badarg),
    Char = unhex(L) bor (unhex(H) bsl 4),
    unquote_bin_(Rest, <<Acc/binary, Char>>);

unquote_bin_(<<Char, Rest/binary>>, Acc) ->
    unquote_bin_(Rest, <<Acc/binary, Char>>).


-spec quote_list(string()) -> string().
quote_list([]) ->
    [];

quote_list([Char|T]) ->
    case is_safe(Char) of
        true ->
            [Char|quote_list(T)];
        false ->
            HH = tohex(Char bsr 4),
            HL = tohex(Char band 15),
            [$%, HH, HL|quote_list(T)]
    end.


-spec quote_bin(binary()) -> binary().
quote_bin(String) ->
    quote_bin(String, <<>>).

quote_bin(<<>>, Acc) ->
    Acc;
quote_bin(<<Char, Rest/binary>>, Acc) ->
    case is_safe(Char) of
        true ->
            quote_bin(Rest, <<Acc/binary, Char>>);
        false ->
            HH = tohex(Char bsr 4),
            LH = tohex(Char band 15),
            quote_bin(Rest, <<Acc/binary, $%, HH, LH>>)
    end.


unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(C) when C >= $0, C =< $9 -> C - $0.

tohex(C) when C < 10 -> $0 + C;
tohex(C) when C < 16 -> $A + (C - 10).


is_hex(C) when C >= $0, C =< $9 -> true;
is_hex(C) when C >= $a, C =< $f -> true;
is_hex(C) when C >= $A, C =< $F -> true;
is_hex(_) -> false.

is_safe($.) -> true;
is_safe($-) -> true;
is_safe($~) -> true;
is_safe($_) -> true;
is_safe(C) when C >= $a, C =< $z -> true;
is_safe(C) when C >= $A, C =< $Z -> true;
is_safe(C) when C >= $0, C =< $9 -> true;
is_safe(_) -> false.
