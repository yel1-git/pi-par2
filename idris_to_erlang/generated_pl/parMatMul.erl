-module(parMatMul).
-compile(export_all).

-import(play2, [process/1, app_stream/2, sync_stream/1, toPListWithChunk/3, syncPList/1]).

get_heads([]) -> [];
get_heads([R|Rs]) -> [hd(R) | get_heads(Rs)].

get_tails([]) -> [];
get_tails([R|Rs]) -> [tl(R) | get_tails(Rs)].

transpose1(_, []) -> [];
transpose1(_, [[]|_]) -> [];
transpose1(N, Rows) ->
    [get_heads(Rows) | transpose1(N, get_tails(Rows))].

dot_product([], []) -> 0;
dot_product([A|As], [B|Bs]) -> A * B + dot_product(As, Bs).

multiply_row_by_col(_, []) -> [];
multiply_row_by_col(Row, [Col|Cols]) ->
    [dot_product(Row, Col) | multiply_row_by_col(Row, Cols)].

multiply_internal([], _) -> [];
multiply_internal([Row|Rows], B) ->
    [multiply_row_by_col(Row, B) | multiply_internal(Rows, B)].

multiply(N, A, B) -> multiply_internal(A, transpose1(N, B)).

rangeFrom(Start, 0) -> [];
rangeFrom(Start, K) when K > 0 -> [Start | rangeFrom(Start+1, K-1)].

computeChunk({Chunk, MatB}) ->
    lists:map(fun(Row) -> multiply_row_by_col(Row, MatB) end, Chunk).

parMatMul(ChunkSize, MatA, MatB) ->
    TransposedB = transpose1(ChunkSize, MatB),
    F = fun(Chunk) -> ?MODULE:computeChunk({Chunk, TransposedB}) end,
    PList = toPListWithChunk(F, ChunkSize, MatA),
    Results = syncPList(PList),
    lists:append(Results).

run(Nw, Size) ->
    Row = rangeFrom(1, Size),
    MatA = lists:duplicate(Size, Row),
    MatB = MatA,
    parMatMul(Nw, MatA, MatB).
