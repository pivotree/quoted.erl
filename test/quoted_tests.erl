-module(quoted_tests).
-include_lib("eunit/include/eunit.hrl").
-ifdef(PROPER).
-undef(LET).
-include_lib("proper/include/proper.hrl").
-endif.
-define(q, quoted).

space_char_test_() ->
    [?_assertEqual(" ", ?q:to_list("%20")),
     ?_assertEqual(" ", ?q:to_list(<<"%20">>)),
     ?_assertEqual(<<" ">>, ?q:to_binary("%20")),
     ?_assertEqual(<<" ">>, ?q:to_binary(<<"%20">>)),
     ?_assertEqual("%20", ?q:as_list(" ")),
     ?_assertEqual("%20", ?q:as_list(<<" ">>)),
     ?_assertEqual(<<"%20">>, ?q:as_binary(" ")),
     ?_assertEqual(<<"%20">>, ?q:as_binary(" "))].

-ifdef(PROPER).

bstring() ->
    list(byte()).

list_inv_prop() ->
    ?FORALL(Input, bstring(),
    begin
        Quoted = ?q:as_list(Input),
        Unquoted = ?q:to_list(Quoted),
        Unquoted == Input
    end).

list_inv_test() -> ?assert(proper:quickcheck(list_inv_prop())).

-endif.
