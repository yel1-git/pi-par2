-module(parCpi).
-compile(export_all).

-import(play2, [process/1, app_stream/2, sync_stream/1, toPListWithChunk/3, syncPList/1]).

mapRedr(G,E,F,Xs) -> lists:foldr( G  , E  ,  ( lists:map(F, Xs ) )  ) .

f(X) -> 4  /  ( 1  + X  * X  ) .

index(I) -> I  - 0.5 .

index2(I,N) -> ?MODULE:index( I  )  / N .

cpi(N) -> ?MODULE:mapRedr( fun erlang:'+'/2  , 0  ,  ( fun ( I ) -> ?MODULE:f(  ( ?MODULE:index2( I  , N  )  )  )  end )  , lists:seq( 1  , N  )  )  / N .

computeChunk(Chunk, N) ->
    lists:foldr(fun(I, Acc) ->
        X = (I - 0.5) / N,
        (4.0 / (1.0 + X * X) / N) + Acc
    end, 0.0, Chunk).

parCpi(ChunkSize, N, Input) ->
    F = fun(Chunk) -> ?MODULE:computeChunk(Chunk, N) end,
    PList = toPListWithChunk(F, ChunkSize, Input),
    Results = syncPList(PList),
    lists:foldr(fun erlang:'+'/2, 0.0, Results).

run(Nw, N) ->
    erlang:system_flag(schedulers_online, Nw),
    Input = lists:seq(1, N),
    ChunkSize = N div Nw,
    io:format("CPI ~p workers: ~p~n", [Nw, sk_profile:benchmark(fun parCpi/3, [ChunkSize, N, Input], 1)]).

run_seq(N) ->
    io:format("CPI seq: ~p~n", [sk_profile:benchmark(fun cpi/1, [N], 1)]).
