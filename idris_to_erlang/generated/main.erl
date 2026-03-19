-module(main).
-compile(export_all).

parMapFol(F,PNilChkHom) -> pnil ;
parMapFol(F,(Hd,Tl)) -> R = ?MODULE:F( Hd  ) ,
T = ?MODULE:parMapFol( F  , Tl  ) ,
?MODULE:PCons( R  , T  ) .

foldr2(F,A,PNil) ->  ( play2:app_stream(play2:process( ( fun ( X ) -> X  end  ) ),A ) ) ;
foldr2(F,A,(Hd,Tl)) -> R =  ( ?MODULE:foldr2( F  , A  , Tl  )  ) ,
?MODULE:<#$$>( F  , Hd  , R  ) ;
foldr2(F,A,PNilChkHom) ->  ( play2:app_stream(play2:process( ( fun ( X ) -> X  end  ) ),A ) ) ;
foldr2(F,A,(Hd,Tl)) -> Hd' = play2:sync_stream3(Hd  , length(Input)),
R =  ( play2:app_stream(play2:process( ( fun ( X ) -> X  end  ) ),lists:foldr( F  , A  , Hd'  ) ) ) ,
Res = ?MODULE:foldr2( F  , A  , Tl  ) ,
U = ?MODULE:<#$$>( F  , R  , Res  ) ,
U .

mapRedr3(G,E,F,N,L) -> ?MODULE:<#++>(  ( L  ?MODULE:<#$> F  )  , G  , E  ) .

vectToPList([]) -> PNilChkHom ;
vectToPList(([X|Xs])) -> Pr = play2:process( ( fun ( X ) -> X  end  ) ),
Pr2 = play2:app_stream2(Pr  , X ),
?MODULE:PConsChkHom( Pr2  ,  ( ?MODULE:vectToPList( Xs  )  )  ) .

parMapRedr2(N,G,E,F,I) -> F' = ?MODULE:mapRedr3( G  , E  , F  ,  ( utils:s( N  )  )  ) ,
Ma = ?MODULE:parMapFol( F'  , I  ) ,
Fo = ?MODULE:foldr2( G  , E  , Ma  ) ,
Fo .

f(X) -> 4  /  ( 1  + X  * X  ) .

index(I) -> I  - 0.5 .

index2(I,N) -> ?MODULE:index( I  )  / N .

parCpi((Nw),N,N2,Prf,V) -> I = ?MODULE:chunk3( V  , N  ,  ( utils:s( Nw-1  )  )  , Prf  ) ,
R = play2:sync_stream(  ( ?MODULE:parMapRedr2(  (  ( ?MODULE:divNat(  ( utils:s( Nw-1  )  )  , N  )  )  )  , fun erlang:'+'/2  , 0  ,  ( fun ( Ind ) -> ?MODULE:f(  ( ?MODULE:index2( Ind  ,  ( ?MODULE:fromInteger( N2  )  )  )  )  )  end  )  ,  ( ?MODULE:vectToPList( I  )  )  )  )  ) ,
 ( utils:snd2( R  ) )  / N2 ;
parCpi(0,_,_,_,_) -> 0.0 .

