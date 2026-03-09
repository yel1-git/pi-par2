-module(parMatMul).
-compile(export_all).

transpose1 ( ([[]|N]) )  ->
        [];
transpose1 ( B )  ->
        [ ( lists:map(  ( fun ( X ) -> hd( X  )  end  )  , B  )  )  | ?MODULE:transpose1(  ( lists:map(  ( fun ( X ) -> tl( X  )  end  )  , B  )  )  ) ]
.

red ( Pair , Sum )  ->
         ( element(1, Pair  )  )  *  ( element(2, Pair  )  )  + Sum
.

dot_product ( A , B )  ->
        lists:foldl( fun red/2  , 0  ,  ( lists:zip( A  , B  )  )  )
.

multiply_row_by_col ( Row , [] )  ->
        [];
multiply_row_by_col ( Row , ([Col_head|Col_rest]) )  ->
        [ ( ?MODULE:dot_product( Row  , Col_head  )  )  |  ( ?MODULE:multiply_row_by_col( Row  , Col_rest  )  ) ]
.

multiply_internal ( [] , B )  ->
        [];
multiply_internal ( ([Head|Rest]) , B )  ->
        [ ( ?MODULE:multiply_row_by_col( Head  , B  )  )  |  ( ?MODULE:multiply_internal( Rest  , B  )  ) ]
.

multiply ( A , B )  ->
        ?MODULE:multiply_internal( A  ,  ( ?MODULE:transpose1( B  )  )  )
.

mkMsg ( [] )  ->
        [mend ];
mkMsg ( ([X|Xs]) )  ->
        [{msg ,X } | ?MODULE:mkMsg( Xs  ) ]
.

farmIt ( Nw , MatA , MatB )  ->
        play2:taskFarm(fun(M) -> ?MODULE:multiply_row_by_col(M, MatB) end, Nw, MatA ). 


run( Nw, Size) ->
	erlang:system_flag(schedulers_online, Nw),
	MatA =  lists:duplicate(Size, lists:seq(1,Size)),
	MatB = parMatMul:transpose1(lists:duplicate(Size, lists:seq(1,Size))),
	io:format("MatMul ~p~n", [sk_profile:benchmark(fun ?MODULE:farmIt/3, [Nw, MatA, MatB], 1)]),
	io:format("Done with examples on ~p cores.~n--------~n", [Nw]).


