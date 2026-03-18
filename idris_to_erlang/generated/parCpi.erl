-module(parCpi).
-compile(export_all).

mapRedr(G,E,F,Xs) -> lists:foldr( G  , E  ,  ( lists:map(F, Xs ) )  ) .

f(X) -> 4  /  ( 1  + X  * X  ) .

index(I) -> I  - 0.5 .

index2(I,N) -> ?MODULE:index( I  )  / N .

cpi(N) -> ?MODULE:mapRedr( fun(X,Y) -> X + Y end , 0  ,  ( fun ( I ) -> ?MODULE:f(  ( ?MODULE:index2( I  , N  )  )  )  end  )  , lists:seq( 1  , N  )  )  / N .

