-module(parSumEuler2).
-compile(export_all).

gcd2 ( A , 0 )  ->
        A ;
gcd2 ( A , B )  ->
        ?MODULE:gcd2( B  ,  ( A  rem B  )  )
.
relPrime ( X , Y )  ->
         ( ?MODULE:gcd2( X  , Y  )  )  == 1
.
mkList ( N )  ->
        lists:seq(      1  ,    N  )
.
euler ( N )  ->
        length(  ( lists:filter(  ( fun ( X ) -> ?MODULE:relPrime( N  , X  )  end  )  ,  ( ?MODULE:mkList( N  )  )  )  )  )
.
sumEuler ( N )  ->
        lists:sum(  ( lists:map(  ( fun ( X ) -> ?MODULE:euler(  ( X  )  )  end  )  ,  ( ?MODULE:mkList( N  )  )  )  )  )
.


farm4RR ( Nw , Input )  ->

	lists:sum(play2:taskFarm(fun(M) -> ?MODULE:euler(M) end, Nw, Input)).


run_examples( Nw, Size) ->
      erlang:system_flag(schedulers_online, Nw),
      L = ?MODULE:mkList(Size),
      io:format("SumEuler: ~p~n", [sk_profile:benchmark(fun ?MODULE:farm4RR/2, [Nw, L], 1)]),
      io:format("Done with examples on ~p cores.~n------~n", [Nw]).
