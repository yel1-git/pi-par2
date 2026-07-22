-module(play_sync).
-compile(export_all).

%% Instrumented drop-in replacement for play2.erl. Same public API and
%% semantics; adds counters for the costs a reviewer would call
%% "synchronization overhead" that play2.erl doesn't expose on its own:
%%
%%   compute_time_us  - time worker processes spend inside the user Fun(M)
%%   wait_time_us     - time the coordinator blocks in sync_stream/2 waiting
%%                       for a worker's reply (the actual synchronization cost)
%%   spawn_time_us    - time spent in spawn/3 setting up worker processes
%%   spawn_count      - number of processes spawned
%%   message_count    - number of unit-of-work messages dispatched via app_stream
%%   message_bytes    - approx bytes copied for those messages (erlang:external_size),
%%                       a proxy for inter-process copying/memory cost
%%
%% To use on a benchmark, change its `-import(play2, [...])` (or `play2:`
%% qualifiers) to play_sync, then wrap the run call in play_sync:profile/1
%% or play_sync:report/1 to reset counters, run, and collect the stats
%% alongside the result.

%% ---- stats collection ----

stats_keys() ->
    [compute_time_us, wait_time_us, spawn_time_us,
     spawn_count, message_count, message_bytes].

ensure_table() ->
    case ets:info(play_sync_stats) of
        undefined ->
            try ets:new(play_sync_stats, [set, public, named_table]) of
                _ -> reset()
            catch
                error:badarg -> ok % created concurrently by another process
            end;
        _ -> ok
    end.

reset() ->
    ensure_table(),
    [ets:insert(play_sync_stats, {K, 0}) || K <- stats_keys()],
    ok.

add(Key, Delta) ->
    ensure_table(),
    ets:update_counter(play_sync_stats, Key, Delta).

stats() ->
    ensure_table(),
    [{K, ets:lookup_element(play_sync_stats, K, 2)} || K <- stats_keys()].

%% Resets counters, runs Fun0/0, and returns {Result, Stats} where Stats
%% also includes wall_time_us for the whole call.
profile(Fun0) ->
    reset(),
    T0 = erlang:monotonic_time(),
    Result = Fun0(),
    T1 = erlang:monotonic_time(),
    WallUs = erlang:convert_time_unit(T1 - T0, native, microsecond),
    {Result, [{wall_time_us, WallUs} | stats()]}.

%% Same as profile/1 but also prints the stats.
report(Fun0) ->
    {Result, Stats} = profile(Fun0),
    io:format("play_sync stats: ~p~n", [Stats]),
    {Result, Stats}.

%% ---- timing helper ----

timed(Fun0) ->
    T0 = erlang:monotonic_time(),
    Result = Fun0(),
    T1 = erlang:monotonic_time(),
    {Result, erlang:convert_time_unit(T1 - T0, native, microsecond)}.

%% ---- instrumented core primitives (mirrors play2.erl) ----

drain_stream2([]) ->
    receive
        {sus_data, M} ->
            drain_stream2([M]);
        {release, Pid} ->
            Pid ! [],
            drain_stream2([])
    end;
drain_stream2([R | Results]) ->
    receive
        {sus_data, M} ->
            drain_stream2([R | Results] ++ [M]);
        {release, Pid} ->
            Pid ! R,
            drain_stream2(Results)
    end.

drain_stream() ->
    receive
        {sus_data, M} ->
            drain_stream2([M])
    end.

process_function_stream(Fun, Sus) ->
    receive
        stop -> stop;
        {proc_data, M} ->
            {R, ComputeUs} = timed(fun() -> Fun(M) end),
            add(compute_time_us, ComputeUs),
            Sus ! {sus_data, R},
            process_function_stream(Fun, Sus)
    end.

% process : (f : [a] -> [b]) -> Process [a] [b]
process(F) ->
    {{Sus, Pid}, SpawnUs} = timed(fun() ->
        S = spawn(play_sync, drain_stream, []),
        P = spawn(play_sync, process_function_stream, [F, S]),
        {S, P}
    end),
    add(spawn_time_us, SpawnUs),
    add(spawn_count, 2),
    {Pid, Sus}.

compF({Pid1, Sus1}, {Pid2, Sus2}, Sus) ->
    receive
        stop2 ->
            Pid1 ! stop,
            Pid2 ! stop;
        {proc_data2, M} ->
            Pid1 ! {proc_data, M},
            M1 = sync_stream(Sus1),
            Pid2 ! {proc_data, M1},
            M2 = sync_stream(Sus2),
            Sus ! {sus_data, (M2)},
            compF({Pid1, Sus1}, {Pid2, Sus2}, Sus)
    end.

distributorS(_Pid, []) -> ok;
distributorS(Pid, [M | Ms]) ->
    Pid ! {proc_data, M},
    distributorS(Pid, Ms).

distributor([Pid | Pids]) ->
    receive
        M ->
            Pid ! {proc_data, M},
            distributor(lists:append(Pids, [Pid]))
    end.

processN2(0, _F, _Sus) -> [];
processN2(N, F, Sus) ->
    {Pid, SpawnUs} = timed(fun() -> spawn(play_sync, process_function_stream, [F, Sus]) end),
    add(spawn_time_us, SpawnUs),
    add(spawn_count, 1),
    [Pid | processN2((N - 1), F, Sus)].

% processN : (n : Nat) -> (f : [a] -> [b]) -> Processes [a] [b]
processN(N, F) ->
    {Sus, SpawnUs} = timed(fun() -> spawn(play_sync, drain_stream, []) end),
    add(spawn_time_us, SpawnUs),
    add(spawn_count, 1),
    Pids = processN2(N, F, Sus),
    {Distr, SpawnUs2} = timed(fun() -> spawn(play_sync, distributor, [Pids]) end),
    add(spawn_time_us, SpawnUs2),
    add(spawn_count, 1),
    {Pids, Sus, Distr}.

% app_stream : Process [a] [b] -> a -> Sus b
% <#>
app_stream({Pid, Sus}, X) ->
    add(message_count, 1),
    add(message_bytes, erlang:external_size(X)),
    Pid ! {proc_data, X},
    Sus.

% app_stream : Process a b -> [a] -> Sus [b]
% <##>
app_stream2({Pid, Sus}, []) ->
    Pid ! stop2,
    Sus;
app_stream2({Pid, Sus}, [X | Xs]) ->
    add(message_count, 1),
    add(message_bytes, erlang:external_size(X)),
    Pid ! {proc_data2, X},
    app_stream2({Pid, Sus}, Xs).

% sync_stream : Sus b -> b
sync_stream(Sus) ->
    Sus ! {release, self()},
    T0 = erlang:monotonic_time(),
    receive
        M ->
            add(wait_time_us, erlang:convert_time_unit(erlang:monotonic_time() - T0, native, microsecond)),
            M
    end.

% sync_stream2 : Sus [b] -> [b]
% needs a vector with a size
sync_stream2(_Sus, 0) -> [];
sync_stream2(Sus, N) ->
    Sus ! {release, self()},
    T0 = erlang:monotonic_time(),
    receive
        stop ->
            add(wait_time_us, erlang:convert_time_unit(erlang:monotonic_time() - T0, native, microsecond)),
            [];
        [] ->
            add(wait_time_us, erlang:convert_time_unit(erlang:monotonic_time() - T0, native, microsecond)),
            sync_stream2(Sus, N);
        M ->
            add(wait_time_us, erlang:convert_time_unit(erlang:monotonic_time() - T0, native, microsecond)),
            [M | sync_stream2(Sus, N - 1)]
    end.

sync_stream3(Sus, N) ->
    R = sync_stream2(Sus, N),
    fromList(R).

% distribute : Processes [a] [b] -> a -> Sus [b]
distribute({_Pids, _Sus, Distr}, X) ->
    Distr ! X.

% <###>
% distributeL : Processes [a] [b] -> [a] -> Sus [b]
distributeL({_Pids, Sus, _Distr}, []) ->
    Sus;
distributeL({Pids, Sus, Distr}, [X | Xs]) ->
    Distr ! X,
    distributeL({Pids, Sus, Distr}, Xs).

taskFarm(F, Nw, Inputs) ->
    Workers = processN(Nw, F),
    sync_stream2(distributeL(Workers, Inputs), length(Inputs)).

% connect : Processes [a] [b] -> Sus [a] -> Sus [b]
connect({_Pids, Sus, _Distr}, _Sus2, 0) -> Sus;
connect({Pids, Sus, Distr}, Sus2, N) ->
    R = sync_stream2(Sus2, 1),
    distributeL({Pids, Sus, Distr}, R),
    connect({Pids, Sus, Distr}, Sus2, N - 1).

nest(F1, Nw1, F2, Nw2, Inputs) ->
    Farm1 = processN(Nw1, F1),
    SusF1 = distributeL(Farm1, Inputs),
    Farm2 = processN(Nw2, F2),
    SusF2 = connect(Farm2, SusF1, length(Inputs)),
    sync_stream2(SusF2, length(Inputs)).

toList([]) -> [];
toList([X | Xs]) -> [[X] | toList(Xs)].

fromList([]) -> [];
fromList([[X] | Xs]) -> [X | fromList(Xs)].

comp2([], _Inputs, Sus) -> Sus;
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

parMap(_F, []) -> [];
parMap(F, [X | Xs]) ->
    [app_stream(process(F), X) | parMap(F, Xs)].

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

fibDC([0, _T]) -> 0;
fibDC([1, _T]) -> 1;
fibDC([N, T]) when N < T -> fib(N);
fibDC([N, T]) when N >= T ->
    S1 = app_stream(process(fun fibDC/1), [N - 1, T]),
    S2 = app_stream(process(fun fibDC/1), [N - 2, T]),
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
