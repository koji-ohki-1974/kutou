-module(input).
-export([execute/1]).

-include("vm_if.hrl").
-include("tokens_if.hrl").

-import(vm, [vm/1]).

execute(Fname) ->
    {ok, Data} = file:read_file(Fname),
    Prog = binary_to_list(Data),
%%    io:format("~p~n", [length(Prog)]),
    Tokens = lists:reverse(tokenise(Prog, [])),
%%    io:format("~p~n", [length(Tokens)]),
    Runtime = lists:reverse(parse(Tokens, [])),
%%    io:format("---~n"),
    vm(#vmstate{
        program = Runtime,
        valstack = [],
        callstack = [],
        memory = [],
        pcounter = 0
    }).

tokenise([], Acc) -> Acc;
tokenise([?A|XS], Acc) -> tokenise(XS, [?A|Acc]);
tokenise([?B|XS], Acc) -> tokenise(XS, [?B|Acc]);
tokenise([?C|XS], Acc) -> tokenise(XS, [?C|Acc]);
tokenise([_|XS], Acc) -> tokenise(XS, Acc).

parse([], Acc) -> Acc;
parse([?A,?A|XS], Acc) ->
    {Num, Rest} = parse_number(XS),
%%    io:format("Push:~p~n", [Num]),
    parse(Rest, [['Push', Num]|Acc]);
parse([?A,?C,?A|XS], Acc) ->
%%    io:format("Dup~n"),
    parse(XS, ['Dup'|Acc]);
parse([?A,?B,?A|XS], Acc) ->
    {Num, Rest} = parse_number(XS),
%%    io:format("Ref:~p~n", Num),
    parse(Rest, [['Ref', Num]|Acc]);
parse([?A,?B,?C|XS], Acc) ->
    {Num, Rest} = parse_number(XS),
%%    io:format("Slide:~p~n", Num),
    parse(Rest, [['Slide', Num]|Acc]);
parse([?A,?C,?B|XS], Acc) ->
%%    io:format("Dup~n"),
    parse(XS, ['Swap'|Acc]);
parse([?A,?C,?C|XS], Acc) ->
%%    io:format("Discard~n"),
    parse(XS, ['Discard'|Acc]);

parse([?B,?A,?A,?A|XS], Acc) ->
%%    io:format("Infix:Plus~n"),
    parse(XS, [['Infix', 'Plus']|Acc]);
parse([?B,?A,?A,?B|XS], Acc) ->
%%    io:format("Infix:Minus~n"),
    parse(XS, [['Infix', 'Minus']|Acc]);
parse([?B,?A,?A,?C|XS], Acc) ->
%%    io:format("Infix:Times~n"),
    parse(XS, [['Infix', 'Times']|Acc]);
parse([?B,?A,?B,?A|XS], Acc) ->
%%    io:format("Infix:Divide~n"),
    parse(XS, [['Infix', 'Divide']|Acc]);
parse([?B,?A,?B,?B|XS], Acc) ->
%%    io:format("Infix:Modulo~n"),
    parse(XS, [['Infix', 'Modulo']|Acc]);

parse([?B,?B,?A|XS], Acc) ->
%%    io:format("Store~n"),
    parse(XS, ['Store'|Acc]);
parse([?B,?B,?B|XS], Acc) ->
%%    io:format("Retrieve~n"),
    parse(XS, ['Retrieve'|Acc]);

parse([?C,?A,?A|XS], Acc) ->
    {String,Rest} = parse_string(XS),
%%    io:format("Label:(" ++ String ++ ")~n"),
    parse(Rest, [['Label', String]|Acc]);
parse([?C,?A,?B|XS], Acc) ->
    {String,Rest} = parse_string(XS),
%%    io:format("Call:(" ++ String ++ ")~n"),
    parse(Rest, [['Call', String]|Acc]);
parse([?C,?A,?C|XS], Acc) ->
    {String,Rest} = parse_string(XS),
%%    io:format("Jump:(" ++ String ++ ")~n"),
    parse(Rest, [['Jump', String]|Acc]);
parse([?C,?B,?A|XS], Acc) ->
    {String,Rest} = parse_string(XS),
%%    io:format("If:Zero:(" ++ String ++ ")~n"),
    parse(Rest, [['If', 'Zero', String]|Acc]);
parse([?C,?B,?B|XS], Acc) ->
    {String,Rest} = parse_string(XS),
%%    io:format("If:Negative:(" ++ String ++ ")~n"),
    parse(Rest, [['If', 'Negative', String]|Acc]);

parse([?C,?B,?C|XS], Acc) ->
%%    io:format("Return~n"),
    parse(XS, ['Return'|Acc]);
parse([?C,?C,?C|XS], Acc) ->
%%    io:format("End~n"),
    parse(XS, ['End'|Acc]);

parse([?B,?C,?A,?A|XS], Acc) ->
%%    io:format("OutputChar~n"),
    parse(XS, ['OutputChar'|Acc]);
parse([?B,?C,?A,?B|XS], Acc) ->
%%    io:format("OutputNum~n"),
    parse(XS, ['OutputNum'|Acc]);
parse([?B,?C,?B,?A|XS], Acc) ->
%%    io:format("ReadChar~n"),
    parse(XS, ['ReadChar'|Acc]);
parse([?B,?C,?B,?B|XS], Acc) ->
%%    io:format("ReadNum~n"),
    parse(XS, ['ReadNum'|Acc]);

parse(_, _) -> error("Unrecognised input").

parse_number(Ts) -> parse_num_(Ts, []).

parse_num_([?C|Rest], Acc) -> {make_number(Acc), Rest};
parse_num_([X|Rest], Acc) -> parse_num_(Rest, [X|Acc]).

parse_string(Ts) -> parse_str_(Ts, []).

parse_str_([?C|Rest], Acc) -> {make_string(Acc), Rest};
parse_str_([X|Rest], Acc) -> parse_str_(Rest, [X|Acc]).

make_number(T) ->
    L = lists:last(T),
    if
        L == ?A ->
            make_number_(lists:droplast(T), 1);
        true ->
            -(make_number_(lists:droplast(T), 1))
    end.

make_number_([], _) -> 0;
make_number_([?A|Rest], Pow) -> make_number_(Rest, Pow*2);
make_number_([?B|Rest], Pow) -> Pow + make_number_(Rest, Pow*2).

make_string([]) -> "";
make_string([T|TS]) -> [T|make_string(TS)].
