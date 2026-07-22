-module(fib_sync).
-compile(export_all).

%% Instrumented copy of reference/fib.erl -- see parCpi_sync.erl for the
%% rationale. fib.erl calls play2:process/app_stream/sync_stream directly
%% (no -import), so those call sites are qualified with play_sync instead;
%% runFibDC/3 wraps the existing sk_profile benchmark call in
%% play_sync:profile/1 so both the original timing output and the new
%% synchronization/spawn/message stats come from the same run.

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

fibDC([0, _T]) -> 0;
fibDC([1, _T]) -> 1;
fibDC([N, T]) when N < T -> fib(N);
fibDC([N, T]) when N >= T ->
    S1 = play_sync:app_stream(play_sync:process(fun fibDC/1), [N-1, T]),
    S2 = play_sync:app_stream(play_sync:process(fun fibDC/1), [N-2, T]),
    play_sync:sync_stream(S1) + play_sync:sync_stream(S2).

runFibSeq(Size) ->
   io:format("fib seq ~p~n", [sk_profile:benchmark(fun ?MODULE:fib/1, [Size], 1)]).

runFibDC(Nw, Thres, Size) ->
   erlang:system_flag(schedulers_online, Nw),
   {BenchResult, SyncStats} = play_sync:profile(fun() ->
       sk_profile:benchmark(fun ?MODULE:fibDC/1, [[Size, Thres]], 1)
   end),
   io:format("fib par ~p~n", [BenchResult]),
   io:format("fib ~p workers sync stats: ~p~n", [Nw, SyncStats]),
   io:format("Done with examples on ~p cores.~n--------~n", [Nw]).
