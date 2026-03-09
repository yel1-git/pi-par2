-module(queens2).

-compile(export_all).


check ( {C,L} , {I,J} )  -> 
	 ( L  == J  )  or  (  ( C  + L  )  ==  ( I  + J  )  )  or  (  ( ( C - L  )  )  ==  ( ( I - J  )  )  ) .

safe ( P , N )  -> 
	lists:foldr( fun(X,Y) -> X and Y end  , true  ,  ( [not(  ( ?MODULE:check( {i ,j } , {length( P  )  + 1 ,N } )  )  )  || {I,J} <-  ( lists:zip( lists:seq( 1  , length( P  )  )  , P  )  ) ] )  ) .

rainhas2 ( 0 , Linha , Numero )  -> 
	[[]];
rainhas2 ( M , Linha , Numero )  -> 
[lists:append(p  , [n ]) || P <- ?MODULE:rainhas2(  ( ( M - 1  )  )  , Linha  , Numero  ) ,N <-  ( lists:append(lists:seq( Linha  , Numero  )  , lists:seq( 1  , ( Linha - 1  )  ) ) ) ,?MODULE:safe( P  , N  ) ].

prainhas ( Numero , Linha )  -> 
	?MODULE:rainhas2( Numero  , Linha  , Numero  ) .

search ( Numero , N )  -> 
	lists:takewhile(  ( fun ( A ) -> hd( A  )  == N  end  )  ,  ( ?MODULE:prainhas( Numero  , N  )  )  ) .

rainhas ( N )  -> 
	lists:map(  ( fun ( X ) -> ?MODULE:search( N  , X  )  end  )  , lists:seq( 1  , N  )  ) .

run(N) -> 
	  io:format("Queens: ~p~n", [sk_profile:benchmark(fun ?MODULE:rainhas/1, [N], 1)]),
	  io:format("finished~n").


