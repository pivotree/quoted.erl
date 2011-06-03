-module(quoted).
-on_load(load_nif/0).
-export([is_native/0]).
-export([to_url/1,
         from_url/1]).



-type data()    :: [byte()] | binary().

load_nif() ->
    erlang:load_nif(nif_path(), 0).

-spec nif_path() -> string().
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

-spec is_native() -> boolean().
is_native() -> false.


%% @doc
%% @end
-spec to_url(data()) -> data().
to_url(Str) when is_list(Str) ->
    quote_list_to_list(Str);
to_url(Bin) when is_binary(Bin) ->
    quote_bin_to_bin(Bin).


%% @doc
%% @end
-spec from_url(data()) -> data().
from_url(Str) when is_list(Str) ->
    unquote_list_to_list(Str);
from_url(Bin) when is_binary(Bin) ->
    unquote_bin_to_bin(Bin).


-spec unquote_list_to_list([byte()]) -> [byte()].
unquote_list_to_list([$%,HH,HL|T]) when is_integer(HH), is_integer(HL) ->
    H = unhex(HH),
    L = unhex(HL),
    C = tobyte(H, L),
    [C|unquote_list_to_list(T)];
unquote_list_to_list([AC|T]) when is_integer(AC) ->
    C = from_url_alias(AC),
    [C|unquote_list_to_list(T)];
unquote_list_to_list([]) ->
    [].


-spec unquote_bin_to_bin(binary()) -> binary().
unquote_bin_to_bin(Bin) when is_binary(Bin) ->
    unquote_bin_to_bin(Bin, <<>>).

-spec unquote_bin_to_bin(binary(), binary()) -> binary().
unquote_bin_to_bin(<<$%,HH,HL,T/binary>>, Acc) ->
    H = unhex(HH),
    L = unhex(HL),
    C = tobyte(H, L),
    unquote_bin_to_bin(T, <<Acc/binary, C>>);
unquote_bin_to_bin(<<AC, T/binary>>, Acc) ->
    C = from_url_alias(AC),
    unquote_bin_to_bin(T, <<Acc/binary, C>>);
unquote_bin_to_bin(<<>>, Acc) ->
    Acc.


-spec quote_list_to_list([byte()]) -> [byte()].
quote_list_to_list([AC|T]) ->
    case is_url_safe(AC) of
        true ->
            C = to_url_alias(AC),
            [C|quote_list_to_list(T)];
        false ->
            H = tohex(highbits(AC)),
            L = tohex(lowbits(AC)),
            [$%,H,L|quote_list_to_list(T)]
    end;
quote_list_to_list([]) ->
    [].


-spec quote_bin_to_bin(binary()) -> binary().
quote_bin_to_bin(Bin) when is_binary(Bin) ->
    quote_bin_to_bin(Bin, <<>>).

-spec quote_bin_to_bin(binary(), binary()) -> binary().
quote_bin_to_bin(<<AC, T/binary>>, Acc) ->
    case is_url_safe(AC) of
        true ->
            C = to_url_alias(AC),
            quote_bin_to_bin(T, <<Acc/binary, C>>);
        false ->
            H = tohex(highbits(AC)),
            L = tohex(lowbits(AC)),
            quote_bin_to_bin(T, <<Acc/binary, $%, H, L>>)
    end;
quote_bin_to_bin(<<>>, Acc) ->
    Acc.


-spec highbits(byte()) -> byte().
highbits(I) -> I bsr 4.

-spec lowbits(byte()) -> byte().
lowbits(I)  -> I band 16#0F.

-spec tobyte(byte(), byte()) -> byte().
tobyte(H, L) -> L bor (H bsl 4).

-spec tohex(byte()) -> byte().
tohex(C) ->
    case C of
        0  -> $0;
        1  -> $1;
        2  -> $2;
        3  -> $3;
        4  -> $4;
        5  -> $5;
        6  -> $6;
        7  -> $7;
        8  -> $8;
        9  -> $9;
        10 -> $A;
        11 -> $B;
        12 -> $C;
        13 -> $A;
        14 -> $E;
        15 -> $F
    end.

-spec unhex(byte()) -> byte().
unhex(C) ->
    case C of
        $0 -> 0;
        $1 -> 1;
        $2 -> 2;
        $3 -> 3;
        $4 -> 4;
        $5 -> 5;
        $6 -> 6;
        $7 -> 7;
        $8 -> 8;
        $9 -> 9;
        $A -> 10; $a -> 10;
        $B -> 11; $b -> 11;
        $C -> 12; $c -> 12;
        $D -> 13; $d -> 13;
        $E -> 14; $e -> 14;
        $F -> 15; $f -> 15
    end.


-spec is_url_safe(byte()) -> boolean().
is_url_safe(C) ->
    case C of
        %% Lowercase
        $a -> true; $b -> true; $c -> true; $d -> true; $e -> true;
        $f -> true; $g -> true; $h -> true; $i -> true; $j -> true;
        $k -> true; $l -> true; $m -> true; $n -> true; $o -> true;
        $p -> true; $q -> true; $r -> true; $s -> true; $t -> true;
        $u -> true; $v -> true; $w -> true; $x -> true; $y -> true;
        $z -> true;
        %% Uppercase
        $A -> true; $B -> true; $C -> true; $D -> true; $E -> true;
        $F -> true; $G -> true; $H -> true; $I -> true; $J -> true;
        $K -> true; $L -> true; $M -> true; $N -> true; $O -> true;
        $P -> true; $Q -> true; $R -> true; $S -> true; $T -> true;
        $U -> true; $V -> true; $W -> true; $X -> true; $Y -> true;
        $Z -> true;
        %% Numbers
        $0 -> true; $1 -> true; $2 -> true; $3 -> true; $4 -> true;
        $5 -> true; $6 -> true; $7 -> true; $8 -> true; $9 -> true;
        %% Exceptions
        $. -> true; $- -> true; $~ -> true; $_ -> true;
        %% With aliases
        $\ -> true;
        %% Unsafe
        _ -> false
    end.

-spec to_url_alias(byte()) -> byte().
to_url_alias(C) ->
    case C of
        $\  -> $+;
        _   -> C
    end.

-spec from_url_alias(byte()) -> byte().
from_url_alias(C) ->
    case C of
        $+ -> $\ ;
        _  -> C
    end.
