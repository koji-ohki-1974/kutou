-module(kutou).
-export([main/1]).

-import(input, [execute/1]).

main(Args) ->
    if
        length(Args) /= 1 ->
            usage();
        true ->
            execute(hd(Args))
    end.

usage() ->
    io:format("kutou 0.0.1 (c) 2015 Koji Ohki~n"),
    io:format("-------------------------------~n"),
    io:format("Usage:erl -noshell -s kutou main [file] -s init stop~n").
