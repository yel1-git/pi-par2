-module(parSumEuler2).
-compile(export_all).

-import(play2, [process/1, app_stream/2, sync_stream/1, toPListWithChunk/3, syncPList/1]).

gcd2(A,0) -> A;
gcd2(A,B) -> A rem B.

relPrime(X,Y) -> gcd2(X,Y) == 1.

mkList(N) -> lists:seq(1, N).

euler(N) -> length([X || X <- mkList(N), relPrime(N,X)]).

sumEuler(N) -> lists:sum([euler(X) || X <- mkList(N)]).

computeChunk(Chunk) ->
    lists:foldr(fun(A,B) -> A+B end, 0, lists:map(fun euler/1, Chunk)).

parSumEuler(ChunkSize, Input) ->
    F = fun(Chunk) -> ?MODULE:computeChunk(Chunk) end,
    PList = toPListWithChunk(F, ChunkSize, Input),
    Results = syncPList(PList),
    lists:foldr(fun(A,B) -> A+B end, 0, Results).
