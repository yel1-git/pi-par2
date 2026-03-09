-module(parQueens).
-compile(export_all).

check ( {C,L} , {I,J} )  ->
         ( L  == J  )  or  (  ( C  + L  )  ==  ( I  + J  )  )  or  (  ( ( C - L  )  )  ==  ( ( I - J  )  )  )
.
safe ( P , N )  ->
        lists:foldr( fun(X,Y) -> X and Y end  , true  ,  ( [not(  ( ?MODULE:check( {I ,J } , {length( P  )  + 1 ,N } )  )  )  || {I,J} <-  ( lists:zip( lists:seq( 1  , length( P  )  )  , P  )  ) ] )  )
.
rainhas2 ( 0 , Linha , Numero )  ->
        [[]];
rainhas2 ( M , Linha , Numero )  ->
[lists:append( P  , [N ] )  || P <- ?MODULE:rainhas2(  ( ( M  - 1  )  )  , Linha  , Numero  ) ,N <-  ( lists:append(lists:seq( Linha  , Numero  )  , lists:seq( 1  , ( Linha - 1  )  ) ) ) ,?MODULE:safe( P  , N  ) ]
.
prainhas ( Numero , Linha )  ->
        ?MODULE:rainhas2( Numero  , Linha  , Numero  )
.
search ( Numero , N )  ->
        lists:takewhile(  ( fun ( A ) -> hd( A  )  == N  end  )  ,  ( ?MODULE:prainhas( Numero  , N  )  )  )
.
rainhas ( N )  ->
        lists:map(  ( fun ( X ) -> ?MODULE:search( N  , X  )  end  )  , lists:seq( 1  , N  )  )
.

mkMsg (S, [] )  ->
        [ ];
mkMsg (S,  ([X|Xs]) )  ->
        [{S, X } | ?MODULE:mkMsg( S, Xs  ) ].

pRR ( PIn , POut )  ->
        receive
                X -> case X of
         ( {msg,S,M} )  ->
        POut  !  ( ?MODULE:search( S, M  )  ) ,
        Y =     ?MODULE:pRR( PIn  , POut  ) ,
        {};
         ( mend )  ->
        eos
end
        end
.
farm4RR ( Nw , Size, Input )  ->

	play2:taskFarm(fun({S,M}) -> ?MODULE:search(S, M) end, Nw, Input ). 

%        Res =   ?MODULE:spawnN( 0  , Nw  , msgt  , msgt  , pRR  ) ,
%        ?MODULE:roundRobin( msgt  , mkMsg(Size, lists:seq(1,Size))  ,  ( ?MODULE:convertChansRR( Res  )  )  ) ,
%        Msgs =  ?MODULE:roundRobinRec(  ( utils:minus(  Size    , 1  )  )  ,  ( ?MODULE:inChans( Res  )  )  ) ,
%        Msgs
%.

run(Nw, Size) ->
	erlang:system_flag(schedulers_online, Nw),
	Input = mkMsg(Size, lists:seq(1, Size)),
	io:format("Queens: ~p~n", [sk_profile:benchmark(fun ?MODULE:farm4RR/3, [Nw, Size, Input], 1)]),
	io:format("Done with examples on ~p cores.~n--------~n", [Nw]).

