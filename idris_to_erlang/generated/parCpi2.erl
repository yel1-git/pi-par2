-module(parCpi2).
-compile(export_all).

parMapFol(F,pnilchkhom) -> pnil ;
parMapFol(F,{pconschkhom,Hd,Tl}) ->
    R = F( Hd  ) ,
    T = ?MODULE:parMapFol( F  , Tl  ) ,
    {pcons, R, T} .


foldr2(N, F,A,pnil) ->
     ( play2:app_stream(play2:process( ( fun ( X ) -> X  end  ) ),A ) ) ;
foldr2(N, F,A,{pcons, Hd,Tl}) -> R =  ( ?MODULE:foldr2(N, F  , A  , Tl  )  ) ,
                             play2:app_stream_bin( F  , N,  Hd  , R  ) ;
                            foldr2(N, F,A, pnilchkhom) ->  ( play2:app_stream(play2:process( ( fun ( X ) -> X  end  ) ),A ) ) ;
foldr2(N, F,A,{pconschkhom, Hd,Tl}) ->
    Hd2 = play2:sync_stream3(Hd  , N),
    R =  ( play2:app_stream(play2:process( ( fun ( X ) -> X  end  ) ),lists:foldr( F  , A  , Hd2  ) ) ) ,
    Res = ?MODULE:foldr2(N, F  , A  , Tl  ) ,
    U = play2:app_stream_bin( F  , N, R  , Res  ) ,
    U .

mapRedr3(CN, G,E,F,N,L) -> play2:app_fold(CN, play2:app_stream_3(CN, L, F  )  , G  , E  ) .

vectToPList([]) -> nilchkhom ;
vectToPList(([X|Xs])) -> {Pid, Sus} = play2:process( ( fun ( X ) -> X  end  ) ),
                         Pr2 = play2:app_stream2({Pid, Sus}  , X ),
                         {pconschkhom, Pr2  ,  ?MODULE:vectToPList( Xs  ) } .

f(X) -> 4  /  ( 1  + X  * X  ) .

index(I) -> I  - 0.5 .

index2(I,N) -> ?MODULE:index( I  )  / N .

parMapRedr2(N,G,E,F,I) ->
    Ma = ?MODULE:parMapFol( fun(X) -> ?MODULE:mapRedr3(N, G  , E  , F  , N + 1, X  ) end  , I  ) ,
    Fo = ?MODULE:foldr2( N, G  , E  , Ma  ) ,
    Fo .

parCpi(0,_,_,_,_) -> 0.0 ;
parCpi((Nw),N,N2,Prf,V) ->
     I = utils:'unshuffle\''( V  , N  ,  (  Nw   )  , Prf  ) ,
     Sus = ?MODULE:parMapRedr2( erlang:round( N /  utils:s( Nw - 1  ) ) , fun erlang:'+'/2  , 0  ,  fun ( Ind ) -> ?MODULE:f( ?MODULE:index2( Ind  , N2  )  )  end  ,  ?MODULE:vectToPList( I  ) ) ,
     R = play2:sync_stream( Sus ),
     R / N2 .
