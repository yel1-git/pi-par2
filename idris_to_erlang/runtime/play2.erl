-module(play2).
-compile(export_all).


drain_stream2( []) -> 
  receive 
    {sus_data, M} -> 
        % io:format("drain2 received ~p ~n", [M]),
        drain_stream2( [M]);
    {release, Pid} ->
        % io:format("drain2 releasing empty ~n", []),
        Pid ! [],
        drain_stream2([])
  end;
drain_stream2([R | Results]) -> 
  receive 
    {sus_data, M} -> 
        % io:format("drain2 received ~p ~n", [M]),
        drain_stream2( [R | Results] ++ [M]);
    {release, Pid} ->
        % io:format("drain2 releasing ~p ~n", [R]),
        Pid ! R,
        drain_stream2(Results)
  end.


drain_stream() -> 
  receive 
    {sus_data, M} -> 
        %io:format("drain received ~p ~n", [M]),
        drain_stream2( [M])
  end.


process_function_stream(Fun, Sus) ->
    receive
        stop -> stop; 
        {proc_data, M} ->
            %io:format("process_function_stream: ~p~n", [M]),
            Sus ! {sus_data, Fun(M)},
            process_function_stream(Fun, Sus)
    end.


% process : (f : [a] -> [b]) -> Process [a] [b]
process(F) ->
    % io:format("processing ~n", []),
    Sus = spawn(play2, drain_stream, []),
    Pid = spawn(play2, process_function_stream, [F, Sus]) ,
    % io:format("processed ~n", []),
    {Pid, Sus}.

compF({Pid1, Sus1}, {Pid2, Sus2}, Sus) ->
    io:format("compF! ~n", []),
    receive
        stop2 -> 
            Pid1 ! stop,
            Pid2 ! stop;
        {proc_data2, M} ->
            io:format("compF received ~p ~n", [M]),
            Pid1 ! {proc_data, M},
            M1 = sync_stream(Sus1),
            io:format("compF received2 ~p ~n", [M1]),
            Pid2 ! {proc_data, M1},
            M2 = sync_stream(Sus2),
            io:format("compF received3 ~p ~n", [M2]),
            Sus ! {sus_data, (M2)},
            compF({Pid1, Sus1}, {Pid2, Sus2}, Sus)
    end.

% comp : Proc a b -> Proc b c -> Proc a c 
%comp({Pid1, Sus1}, {Pid2, Sus2}) ->
%    io:format("comp! ~n", []),
%    Sus = spawn(play2, drain_stream, []),
%    Pid = spawn(play2, compF, [{Pid1, Sus1}, {Pid2, Sus2}, Sus]),
%    {Pid, Sus}.





distributorS(Pid, []) -> ok;
distributorS(Pid, [M | Ms]) -> Pid ! {proc_data, M},
                               distributorS(Pid, Ms).

distributor([Pid | Pids]) ->
    receive
        M -> % io:format("distributor received: ~p~n", [M]),
             Pid ! {proc_data, M},
             distributor( lists:append(Pids, [Pid]))
    end.

processN2 (0, F, Sus) -> [];
processN2 (N, F, Sus) ->
    % io:format("creating farm process: ~n", []),
    Pid = spawn(play2, process_function_stream, [ F, Sus]),
    [Pid | processN2((N-1), F, Sus)].

% processN : (n : Nat) -> (f : [a] -> [b]) -> Processes [a] [b]
processN(N, F) -> 
    Sus = spawn(play2, drain_stream, []),
    Pids = processN2(N, F, Sus),
    Distr = spawn(play2, distributor, [Pids]),
    {Pids, Sus, Distr}.


% app_stream : Process [a] [b] -> a -> Sus b 
% <#>
app_stream({Pid, Sus}, X) ->
    Pid ! {proc_data, X},
    Sus. 

% app_stream : Process a b -> [a] -> Sus [b] 
% <##>
app_stream2({Pid, Sus}, []) -> 
    Pid ! stop2,
    Sus;
app_stream2({Pid, Sus}, [X | Xs]) ->
    io:format("app_stream2 sending ~p ~n", [X]),
    Pid ! {proc_data2, X},
    app_stream2({Pid, Sus}, Xs). 


% sync_stream : Sus b -> b 
sync_stream(Sus) ->
    Sus ! {release, self()},
    receive 
        M ->  % io:format("sync_stream releasing: ~p ~n", [M]),
              M
    end. 

% sync_stream2 : Sus [b] -> [b] 
% needs a vector with a size
sync_stream2(Sus, 0) -> [];
sync_stream2(Sus, N) ->
    Sus ! {release, self()},
    receive 
        stop -> [];
        [] -> sync_stream2(Sus, N);
        M ->  %io:format("sync_stream releasing: ~p ~n", [M]),
              [M | sync_stream2(Sus, N-1)]
    end.

sync_stream3(Sus, N) ->
    R = sync_stream2(Sus, N),
    fromList(R).


% distribute : Processes [a] [b] -> a -> Sus [b]
distribute({Pids, Sus, Distr}, X) ->
    Distr ! X.

% <###>
% distributeL : Processes [a] [b] -> [a] -> Sus [b] 
distributeL ({Pids, Sus, Distr}, []) ->
    Sus;
distributeL ({Pids, Sus, Distr}, [X | Xs]) ->
    Distr ! X,
    distributeL ({Pids, Sus, Distr}, Xs).

taskFarm (F, Nw, Inputs) ->
    Workers = processN(Nw, F),
    sync_stream2(distributeL(Workers, Inputs), length(Inputs)).

% connect : Processes [a] [b] -> Sus [a] -> Sus [b] 
connect({Pids, Sus, Distr}, Sus2, 0) -> Sus;
connect({Pids, Sus, Distr}, Sus2, N) ->
    R = sync_stream2(Sus2,1),
    % io:format ("Sending ~p to Stage 2 ~n", [R]),
    distributeL({Pids, Sus, Distr}, R),
    connect({Pids, Sus, Distr}, Sus2, N-1).



nest (F1, Nw1, F2, Nw2, Inputs) ->
    Farm1 = processN(Nw1, F1),

    SusF1 = distributeL(Farm1, Inputs),

    Farm2 = processN(Nw2, F2),

    SusF2 = connect(Farm2, SusF1, length(Inputs)),

    sync_stream2(SusF2, length(Inputs)).

toList([]) -> [];
toList([X | Xs]) -> [ [X] | toList(Xs)].

fromList([]) -> [];
fromList([ [X] | Xs]) -> [ X | fromList(Xs)].

comp2([], Inputs, Sus) -> Sus;
comp2([F2 | Fs], Inputs, Sus) ->
    Farm2 = processN(1, F2),
    SusF2 = connect(Farm2, Sus, length(Inputs)),
    comp2(Fs, Inputs, SusF2).

comp([F1 | [F2 | Fs]], Inputs) ->
    Inputs2 = toList(Inputs),
    Farm1 = processN(1, F1),

    SusF1 = distributeL(Farm1, Inputs2),

    Farm2 = processN(1, F2),

    SusF2 = connect(Farm2, SusF1, length(Inputs2)),

    comp2(Fs, Inputs2, SusF2).

pipe(Fs, Input) -> sync_stream3(comp(Fs, Input), length(Input)).


parMap(F, []) -> [];
parMap(F, [X|Xs]) -> 
    [ app_stream(process(F), X) | parMap(F, Xs)].

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

fibDC([0, T]) -> 0;
fibDC([1, T]) -> 1;
fibDC([N, T]) when N < T -> fib(N);
fibDC([N, T]) when N >= T ->
    S1 = app_stream(process(fun fibDC/1), [N-1, T]),
    S2 = app_stream(process(fun fibDC/1), [N-2, T]),
    sync_stream(S1) + sync_stream(S2).

%% split list into chunks of size N
split_chunk(_, []) -> {[], []};
split_chunk(0, Xs) -> {[], Xs};
split_chunk(N, Xs) when N > 0 ->
    case Xs of
        [] -> {[], []};
        _ when N >= length(Xs) -> {Xs, []};
        _ -> {Chunk, Rest} = lists:split(N, Xs), {Chunk, Rest}
    end.

%% creates PList (list of Sus PIDs) from chunks
toPListWithChunk(_F, _ChunkSize, []) -> [];
toPListWithChunk(F, ChunkSize, Xs) ->
    {Chunk, Rest} = split_chunk(ChunkSize, Xs),
    P = process(fun(Y) -> [F(Y)] end),
    Sus = app_stream(P, Chunk),
    [Sus | toPListWithChunk(F, ChunkSize, Rest)].

syncPList([]) -> [];
syncPList([Sus | Rest]) ->
    [R] = sync_stream(Sus),
    [R | syncPList(Rest)].
