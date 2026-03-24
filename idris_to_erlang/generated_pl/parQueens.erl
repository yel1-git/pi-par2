-module(parQueens).
-compile(export_all).

-import(play2, [process/1, app_stream/2, sync_stream/1, toPListWithChunk/3, split_chunk/2, syncPList/1]).

check({C,L}, {I,J}) -> (L == J) or (C+L == I+J) or (C-L == I-J).

safe(P, N) ->
    Indices = lists:zip(lists:seq(1, length(P)), P),
    lists:foldr(
        fun(X, Y) -> X and Y end,
        true,
        lists:map(
            fun({I,J}) ->
                not(check({I,J}, {length(P)+1, N}))
            end,
            Indices
        )
    ).

rainhas2(0, _Linha, _Numero) -> [[]];
rainhas2(M, Linha, Numero) ->
    Cols1 = lists:seq(Linha, Numero),
    Cols2 = lists:seq(1, Linha-1),
    Cols = Cols1 ++ Cols2,
    Ps = rainhas2(M-1, Linha, Numero),
    lists:flatmap(
        fun(P) ->
            lists:map(
                fun(N) -> P ++ [N] end,
                lists:filter(fun(N) -> safe(P, N) end, Cols)
            )
        end,
        Ps
    ).

prainhas(Numero, Linha) -> rainhas2(Numero, Linha, Numero).

search(Numero, N) ->
    All = prainhas(Numero, N),
    lists:takewhile(
        fun(A) ->
            case A of
                [X|_] -> X == N;
                _ -> false
            end
        end,
        All
    ).

rainhas(N) -> lists:map(fun(X) -> search(N, X) end, lists:seq(1, N)).

mkMsg(S, []) -> [];
mkMsg(S, [X|Xs]) -> [{S, X} | mkMsg(S, Xs)].

computeChunk(Chunk) ->
    lists:map(fun({S,M}) -> search(S,M) end, Chunk).

parQueens(ChunkSize, Input) ->
    F = fun(Chunk) -> ?MODULE:computeChunk(Chunk) end,
    PList = toPListWithChunk(F, ChunkSize, Input),
    Results = syncPList(PList),
    lists:foldr(fun(A,B) -> A++B end, [], Results).

run(Nw, Size) ->
    Input = mkMsg(Size, lists:seq(1, Size)),
    parQueens(Nw, Input).
