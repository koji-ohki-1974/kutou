-module(vm).
-export([vm/1]).

-include("vm_if.hrl").

%% Stack machine for running whitespace programs


vm(VM) ->
    Prog = VM#vmstate.program,
    PC = VM#vmstate.pcounter,
    Instr = lists:nth(PC+1, Prog),
%%  putStrLn (show stack)
    do_instr(VM#vmstate{pcounter = PC+1}, Instr).

%% Running individual instructions

do_instr(VM, ['Push', N]) ->
%%    io:format("Push:~p~n", [N]),
    Stack = VM#vmstate.valstack,
    vm (VM#vmstate{valstack = [N|Stack]});
do_instr(VM, 'Dup') ->
%%    io:format("Dup~n"),
    [N|Stack] = VM#vmstate.valstack,
    vm (VM#vmstate{valstack = [N,N|Stack]});
do_instr(VM, ['Ref', I]) ->
%%    io:format("Ref:~p~n", [I]),
    Stack = VM#vmstate.valstack,
    vm (VM#vmstate{valstack = [lists:nth(I+1, Stack)|Stack]});
do_instr(VM, ['Slide', I]) ->
%%    io:format("Slide:~p~n", [I]),
    [N|Stack] = VM#vmstate.valstack,
    vm (VM#vmstate{valstack = [N|lists:nthtail(I, Stack)]});
do_instr(VM, 'Swap') ->
%%    io:format("Swap~n"),
    [N,M|Stack] = VM#vmstate.valstack,
    vm (VM#vmstate{valstack = [M,N|Stack]});
do_instr(VM, 'Discard') ->
%%    io:format("Discard~n"),
    [_|Stack] = VM#vmstate.valstack,
    vm (VM#vmstate{valstack = Stack});
do_instr(VM, ['Infix', Op]) ->
%%    io:format("Infix:" ++ atom_to_list(Op) ++ "~n"),
    [Y,X|Stack] = VM#vmstate.valstack,
    vm (VM#vmstate{valstack = [do_op(Op, X, Y)|Stack]});
do_instr(VM, 'OutputChar') ->
%%    io:format("OutputChar~n"),
    [N|Stack] = VM#vmstate.valstack,
    io:put_chars([N]),
    vm (VM#vmstate{valstack = Stack});
do_instr(VM, 'ReadChar') ->
%%    io:format("ReadChar~n"),
    [Loc|Stack] = VM#vmstate.valstack,
    Heap = VM#vmstate.memory,
    [Ch] = io:get_chars("", 1),
    Hp = store(Ch, Loc, Heap),
    vm (VM#vmstate{valstack = Stack, memory = Hp});
do_instr(VM, 'ReadNum') ->
%%    io:format("ReadNum~n"),
    [Loc|Stack] = VM#vmstate.valstack,
    Heap = VM#vmstate.memory,
    Ch = lists:droplast(io:get_line("")),
    Num = list_to_integer(Ch, 10),
    Hp = store(Num, Loc, Heap),
    vm (VM#vmstate{valstack = Stack, memory = Hp});
do_instr(VM, 'OutputNum') ->
%%    io:format("OutputNum~n"),
    [N|Stack] = VM#vmstate.valstack,
    io:put_chars(integer_to_list(N, 10)),
%%	 hFlush stdout
     vm (VM#vmstate{valstack = Stack});
do_instr(VM, ['Label', _]) ->
%%    io:format("Label~n"),
    vm (VM);
do_instr(VM, ['Call', L]) ->
%%    io:format("Call:" ++ L ++ "~n"),
    Prog = VM#vmstate.program,
    CS = VM#vmstate.callstack,
    PC = VM#vmstate.pcounter,
    Loc = find_label(L, Prog),
    vm (VM#vmstate{callstack = [PC|CS], pcounter = Loc});
do_instr(VM, ['Jump', L]) ->
%%    io:format("Jump:" ++ L ++ "~n"),
    Prog = VM#vmstate.program,
    Loc = find_label(L, Prog),
    vm (VM#vmstate{pcounter = Loc});
do_instr(VM, ['If', T, L]) ->
%%    io:format("If:" ++ atom_to_list(T) ++ ":~p~n", [L]),
    Prog = VM#vmstate.program,
    [N|Stack] = VM#vmstate.valstack,
    R = test(T, N),
    if
        R ->
            Loc = find_label(L, Prog),
            vm (VM#vmstate{valstack = Stack, pcounter = Loc});
        true ->
            vm (VM#vmstate{valstack = Stack})
    end;
do_instr(VM, 'Return') ->
%%    io:format("Return~n"),
    [C|CS] = VM#vmstate.callstack,
    vm (VM#vmstate{callstack = CS, pcounter = C});
do_instr(VM, 'Store') ->
%%    io:format("Store~n"),
    [N,Loc|Stack] = VM#vmstate.valstack,
    Heap = VM#vmstate.memory,
    Hp = store(N, Loc, Heap),
    vm (VM#vmstate{valstack = Stack, memory = Hp});
do_instr(VM, 'Retrieve') ->
%%    io:format("Retrieve~n"),
    [Loc|Stack] = VM#vmstate.valstack,
    Heap = VM#vmstate.memory,
    Val = retrieve(Loc, Heap),
    vm(VM#vmstate{valstack = [Val|Stack]});
do_instr(_, 'End') ->
%%    io:format("End~n"),
    empty;
do_instr(_, I) -> error("Can't do " ++ atom_to_list(I)).

do_op('Plus', X, Y) -> X + Y;
do_op('Minus', X, Y) -> X - Y;
do_op('Times', X, Y) -> X * Y;
do_op('Divide', X, Y) -> X div Y;
do_op('Modulo', X, Y) -> X rem Y.

test('Zero', N) -> N==0;
test('Negative', N) -> N<0.

%% Digging out labels from wherever they are

find_label(L, P) -> find_label_(L, P, 0).

find_label_(L, [], _) -> error("Undefined label (" ++ L ++ ")");
find_label_(M, [['Label', L]|_], I) when L == M ->
%%    io:format("found:~p~n", [I]),
    I;
find_label_(M, [_|XS], I) -> find_label_(M, XS, (I+1)).

%% Heap management

retrieve(X, Heap) -> lists:nth(X+1, Heap).

store(X, 0, [_|HS]) -> [X|HS];
store(X, N, [H|HS]) ->
    Hp = store(X, (N-1), HS),
    [H|Hp];
store(X, 0, []) -> [X];
store(X, N, []) ->
    Hp = store(X, (N-1), []),
    [0|Hp].
