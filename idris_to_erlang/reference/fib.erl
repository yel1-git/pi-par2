-module(fib).
-compile(export_all).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

fibDC([0, T]) -> 0;
fibDC([1, T]) -> 1;
fibDC([N, T]) when N < T -> fib(N);
fibDC([N, T]) when N >= T ->
    S1 = play2:app_stream(play2:process(fun fibDC/1), [N-1, T]),
    S2 = play2:app_stream(play2:process(fun fibDC/1), [N-2, T]),
    play2:sync_stream(S1) + play2:sync_stream(S2).

runFibSeq(Size) ->
   io:format("fib seq ~p~n", [sk_profile:benchmark(fun ?MODULE:fib/1, [Size], 1)]).

runFibDC(Nw, Thres, Size) -> 
   erlang:system_flag(schedulers_online, Nw), 
   io:format("fib par ~p~n", [sk_profile:benchmark(fun ?MODULE:fibDC/1, [[Size, Thres]], 1)]),
   io:format("Done with examples on ~p cores.~n--------~n", [Nw]).




