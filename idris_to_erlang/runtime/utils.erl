-module(utils).
-compile(export_all).

fst(A) -> A.

snd(A) -> A.

fst2({A,B}) -> A.

snd2({A,B}) -> B.

n_length_chunks([],Len) -> [];
n_length_chunks(List,Len) -> if Len >  (length(List))  ->
	[List]
; true ->
	{Head,Tail} = lists:split( (Len) ,List),
	[Head | ?MODULE:n_length_chunks(Tail,Len)]
end.

minus(A,B) -> A - B.

divide([],N) -> [];
divide(List,N) -> if  (length(List))  < N ->
	[List]
; true ->
	{Head,Tail} = lists:split( (N) ,List),
	[Head | ?MODULE:divide(Tail,N)]
end.

drop(0,Xs) -> Xs;
drop(N,[]) -> [];
drop(N,[X|Xs]) -> ?MODULE:drop( (N - 1) ,Xs).

takeEach(N,[]) -> [];
takeEach(N,[X|Xs]) -> [X | ?MODULE:takeEach(N, (?MODULE:drop( (N - 1) ,Xs)) )].

unshuffle(N,Xs) -> lists:map( (fun (I) -> ?MODULE:takeEach(N, (?MODULE:drop(I,Xs)) ) end ) , (lists:seq(0,0 + N - 1)) ).

'unshuffle\''(Xs,Nw,Len,P) -> ?MODULE:unshuffle(Nw,Xs).

s(N) -> N + 1.

mkMsg([]) -> [mend];
mkMsg([X|Xs]) -> [{msg,X} | ?MODULE:mkMsg(Xs)].

divLem(X) -> X.

just(X) -> X.


