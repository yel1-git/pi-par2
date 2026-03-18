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

parMapRedr2(N,G,E,F,I) -> S = ?MODULE:splitIntoN2( N  , I  ) ,
F' = ?MODULE:mapRedr3( G  , E  , F  , N  ) ,
Ma = ?MODULE:parMapFol( F'  , S  ) ,
Fo = ?MODULE:foldr2( G  , E  , Ma  ) ,
Fo .

