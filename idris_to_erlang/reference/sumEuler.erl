-module(sumEuler).

-compile(export_all).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).


relPrime(X,Y) -> gcd(X, Y) == 1.

mkList(N) -> lists:seq(1,N).


euler(N) -> length( lists:filter(fun(X) -> ?MODULE:relPrime(N,X) end, mkList(N))).


sumEuler(N) -> io:format("~p~n", [lists:sum(lists:map(fun ?MODULE:euler/1,mkList(N)))]).

sumEulerDCP({T, []}) -> 0;
sumEulerDCP({T, [X]}) -> euler(X);
sumEulerDCP({T, Xs}) when length(Xs) < T -> sumEulerDC(Xs); 
sumEulerDCP({T, Xs}) -> 
	{Left, Right} = lists:split(length(Xs) div 2, Xs),
        S1 = play2:app_stream(play2:process(fun sumEulerDCP/1), {T, Left}),
	S2 = play2:app_stream(play2:process(fun sumEulerDCP/1), {T, Right}),
	play2:sync_stream(S1) + play2:sync_stream(S2).

sumEulerDC([]) -> 0;
sumEulerDC([X]) -> euler(X);
sumEulerDC(Xs) ->
        {Left, Right} = lists:split(length(Xs) div 2, Xs),
        S1 = sumEulerDC(Left),
        S2 = sumEulerDC(Right),
        S1 + S2.

run_seq(X) ->
	  io:format("SumEuler Seq: ~p~n", [sk_profile:benchmark(fun ?MODULE:sumEuler/1, [X], 1)]),
	  io:format("Done with seq. ~n").

run_seq_dc(X) ->
          io:format("SumEuler Seq: ~p~n", [sk_profile:benchmark(fun ?MODULE:sumEulerDC/1, [mkList(X)], 1)]),
          io:format("Done with seq. ~n").

run_examples(Nw, Size, T) ->
   erlang:system_flag(schedulers_online, Nw),
   io:format("sumEuler DC ~p~n", [sk_profile:benchmark(fun ?MODULE:sumEulerDCP/1, [{T, mkList(Size)}], 1)]),
   io:format("Done with examples on ~p cores.~n--------~n", [Nw]).
