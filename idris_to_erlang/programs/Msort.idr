module Msort

import Data.List
import Data.Vect
import Pipar2

%default total

public export
sortMerge : Ord a => List a -> List a -> List a
sortMerge [] ys = ys
sortMerge xs [] = xs
sortMerge (x :: xs) (y :: ys) =
  if x <= y
    then x :: sortMerge xs (y :: ys)
    else y :: sortMerge (x :: xs) ys

public export
split : List x -> (List x, List x) -> (List x, List x)
split [] sofar = sofar
split (a :: as) (fs, ss) = split as (ss, a :: fs)

public export
mergeSort : Ord a => List a -> List a
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let (th, bh) = split xs ([], []) in
    sortMerge (mergeSort th) (mergeSort bh)

public export
mergeSortPar2 : Ord a => (Int, List a) -> List a
mergeSortPar2 (t, []) = []
mergeSortPar2 (t, [x]) = [x]
mergeSortPar2 (t, xs) =
  if (length xs) < t
    then mergeSort xs
    else
      let (th, bh) = split xs ([], []) in
      let r1 = (proc mergeSortPar2) <#> (t, th) in
      let r2 = (proc mergeSortPar2) <#> (t, bh) in
        sortMerge ((<$>) r1) ((<$>) r2)

-- There is an extra argument in the .erl file
public export
mergeSortPar : Ord a => (Int, List a) -> List a
mergeSortPar (t, xs) = mergeSortPar2 (t, xs)

-- random:uniform & lists:seq missing. probably directly implemented in erl for testing
public export
generate_random_int_list : Int -> Int -> Int -> List Int
generate_random_int_list n startVal lim = []

public export
runMergeSeq : Int -> List Int
runMergeSeq size = generate_random_int_list size 0 10000

public export
runFibDC : Int -> Int -> Int -> List Int
runFibDC nw thres size =
  let list = generate_random_int_list size 0 10000
  in mergeSortPar (thres, list)
