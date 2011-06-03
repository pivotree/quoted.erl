-module(quoted_tests).
-include_lib("eunit/include/eunit.hrl").
-ifdef(PROPER).
-undef(LET).
-include_lib("proper/include/proper.hrl").
-endif.
-define(q, quoted).

% Library behaviour:
%   " "<->"%20"
%   " "<--"+"
%
% Simple and more according to standard, but still supports the popular +
% encoding.
space_char_test_() ->
    % " "<--"%20"
    [?_assertEqual(" ", ?q:to_list("%20")),
     ?_assertEqual(" ", ?q:to_list(<<"%20">>)),
     ?_assertEqual(<<" ">>, ?q:to_binary("%20")),
     ?_assertEqual(<<" ">>, ?q:to_binary(<<"%20">>)),
    % " "<--"+"
     ?_assertEqual(" ", ?q:to_list("+")),
     ?_assertEqual(" ", ?q:to_list(<<"+">>)),
     ?_assertEqual(<<" ">>, ?q:to_binary("+")),
     ?_assertEqual(<<" ">>, ?q:to_binary(<<"+">>)),
    % " "-->"%20" 
     ?_assertEqual("%20", ?q:as_list(" ")),
     ?_assertEqual("%20", ?q:as_list(<<" ">>)),
     ?_assertEqual(<<"%20">>, ?q:as_binary(" ")),
     ?_assertEqual(<<"%20">>, ?q:as_binary(<<" ">>))].

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

bin_inv_prop() ->
    ?FORALL(Initinput, bstring(),
    begin
        Input = list_to_binary(Initinput),
        Quoted = ?q:as_binary(Input),
        Unquoted = ?q:to_binary(Quoted),
        Unquoted == Input
    end).

list_via_bin_inv_prop() ->
    ?FORALL(Input, bstring(),
    begin
        Quoted = ?q:as_binary(Input),
        Unquoted = ?q:to_list(Quoted),
        Unquoted == Input
    end).

bin_via_list_inv_prop() ->
    ?FORALL(Initinput, bstring(),
    begin
        Input = list_to_binary(Initinput),
        Quoted = ?q:as_list(Input),
        Unquoted = ?q:to_binary(Quoted),
        Unquoted == Input
    end).


list_inv_test() -> ?assert(proper:quickcheck(list_inv_prop())).
bin_inv_test() -> ?assert(proper:quickcheck(bin_inv_prop())).
list_via_bin_inv_test() -> ?assert(proper:quickcheck(list_via_bin_inv_prop())).
bin_via_list_inv_test() -> ?assert(proper:quickcheck(bin_via_list_inv_prop())).

-endif.
