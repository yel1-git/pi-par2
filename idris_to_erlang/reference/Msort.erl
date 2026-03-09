-module(msort).
-compile(export_all).


sortMerge([], Ylist) -> Ylist;
sortMerge(XList, []) -> XList;
sortMerge([X|Xs], [Y|Ys]) when X =< Y ->
	[X | sortMerge(Xs, [Y|Ys])];
sortMerge([X|Xs], [Y|Ys]) when X >= Y ->
	[Y | sortMerge([X|Xs], Ys)]. 


split([], SoFar) -> SoFar;
split([A|As], {Fs, Ss}) ->
	split(As, {Ss, [A|Fs]}).


mergeSort([]) -> [];
mergeSort([X]) -> [X];
mergeSort(Xs) ->
	{Th, Bh} = split(Xs, {[],[]}),
	sortMerge(mergeSort(Th),mergeSort(Bh)).

mergeSortPar2({T, []}) -> [];
mergeSortPar2({T, [X]}) -> [X];
mergeSortPar2({T, Xs}) when length(Xs) < T -> mergeSort(Xs);
mergeSortPar2({T, Xs})  ->
        {Th, Bh} = split(Xs, {[],[]}),
	S1 = play2:app_stream(play2:process(fun mergeSortPar2/1), {T, Th}),
	S2 = play2:app_stream(play2:process(fun mergeSortPar2/1), {T, Bh}),
	sortMerge(play2:sync_stream(S1), play2:sync_stream(S2)).

mergeSortPar({T, Xs}) -> mergeSortPar2({length(Xs),T,Xs}). 


generate_random_int_list(N,StartVal,Lim) ->
    lists:map(fun (_) -> random:uniform(Lim-StartVal) + StartVal end, lists:seq(1,N)).


runMergeSeq(Size) ->
   List = generate_random_int_list(Size, 0, 10000),
   io:format("fib seq ~p~n", [sk_profile:benchmark(fun ?MODULE:mergeSort/1, [List], 1)]).

runFibDC(Nw, Thres, Size) -> 
   erlang:system_flag(schedulers_online, Nw), 
   List = generate_random_int_list(Size, 0, 10000),
   io:format("Merge Sort ~p~n", [sk_profile:benchmark(fun ?MODULE:mergeSortPar/1, [Size], 1)]),
   io:format("Done with examples on ~p cores.~n--------~n", [Nw]).


