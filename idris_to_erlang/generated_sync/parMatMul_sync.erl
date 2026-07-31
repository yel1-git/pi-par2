-module(parMatMul_sync).
-compile(export_all).

%% Instrumented copy of generated_pl/parMatMul.erl -- see parCpi_sync.erl
%% for the rationale; only the import and run/2 differ from the original.

-import(play_sync, [process/1, app_stream/2, sync_stream/1, toPListWithChunk/3, syncPList/1]).

get_heads([]) -> [];
get_heads([R|Rs]) -> [hd(R) | get_heads(Rs)].

get_tails([]) -> [];
get_tails([R|Rs]) -> [tl(R) | get_tails(Rs)].

fst2 (A) -> element(1, A).

snd2 (A) -> element(2, A).

transpose1 ( ([[]|N]) )  ->
        [];
transpose1 ( B )  ->
        [ ( lists:map(  ( fun ( X ) -> hd( X  )  end  )  , B  )  )  | ?MODULE:transpose1(  ( lists:map(  ( fun ( X ) -> tl( X  )  end  )  , B  )  )  ) ]
.
red ( Pair , Sum )  ->
         ( fst2( Pair  )  )  *  ( snd2( Pair  )  )  + Sum
.
dot_product ( A , B )  ->
        lists:foldl( fun red/2  , 0  ,  ( lists:zip( A  , B  )  )  ).


multiply_row_by_col(_, []) -> [];
multiply_row_by_col(Row, [Col|Cols]) ->
    [dot_product(Row, Col) | multiply_row_by_col(Row, Cols)].

multiply_internal([], _) -> [];
multiply_internal([Row|Rows], B) ->
    [multiply_row_by_col(Row, B) | multiply_internal(Rows, B)].

multiply(N, A, B) -> multiply_internal(A,  B).

rangeFrom(Start, 0) -> [];
rangeFrom(Start, K) when K > 0 -> [Start | rangeFrom(Start+1, K-1)].

computeChunk({Chunk, MatB}) ->
   lists:map(fun(Row) -> multiply_row_by_col(Row, MatB) end, Chunk).

%% MatB arrives ALREADY TRANSPOSED, so that -- as in multiply/3 and run_seq/1 --
%% the O(n^2) transpose sits outside the profiled region.
parMatMul(ChunkSize, MatA, MatB) ->
    F = fun(Chunk) -> ?MODULE:computeChunk({Chunk, MatB}) end,
    PList = toPListWithChunk(F, ChunkSize, MatA),
    Results = syncPList(PList),
    lists:append(Results).

mkRandomMatrix(Size) ->
    rand:seed(exs64, {42,42,42}),
    [[rand:uniform(1000) || _ <- lists:seq(1, Size)] || _ <- lists:seq(1, Size)].

run(Nw, Size) ->
    erlang:system_flag(schedulers_online, Nw),
    Row = rangeFrom(1, Size),
    MatA = mkRandomMatrix(Size),
    MatB = transpose1(MatA),
    ChunkSize = Size div Nw,
    {BenchResult, SyncStats} = play_sync:profile(fun() ->
        sk_profile:benchmark(fun parMatMul/3, [ChunkSize, MatA, MatB], 1)
    end),
    io:format("MatMul ~p workers: ~p~n", [Nw, BenchResult]),
    io:format("MatMul ~p workers sync stats: ~p~n", [Nw, SyncStats]).

run_seq(Size) ->
    Row = rangeFrom(1, Size),
    MatA = mkRandomMatrix(Size),
    MatB = transpose1(MatA),
    io:format("MatMul seq: ~p~n", [sk_profile:benchmark(fun multiply/3, [Size, MatA, MatB], 1)]).
