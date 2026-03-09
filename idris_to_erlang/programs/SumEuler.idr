module SumEuler

import Data.List
import Data.Vect
import Pipar2

public export
gcd : Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

public export
relPrime : Int -> Int -> Bool
relPrime x y = gcd x y == 1

public export
mkList : Int -> List Int
mkList n = [1..n]

public export
euler : Int -> Int
euler n = cast (length (filter (\x => relPrime n x) (mkList n)))

public export
sumEuler : Int -> Int
sumEuler n = sum (map euler (mkList n))

public export
sumEulerDC : List Int -> Int
sumEulerDC [] = 0
sumEulerDC [x] = euler x
sumEulerDC xs =
  let (left, right) = splitAt ((length xs) `div` 2) xs
  in sumEulerDC left + sumEulerDC right

public export
sumEulerDCP : (Int, List Int) -> Int
sumEulerDCP (t, []) = 0
sumEulerDCP (t, [x]) = euler x
sumEulerDCP (t, xs) =
  if (cast (length xs)) < t
    then sumEulerDC xs
    else
      let (left, right) = splitAt ((length xs) `div` 2) xs in
      let p = proc sumEulerDCP in
      let r1 = p <#> (t, left) in
      let r2 = p <#> (t, right) in
      let s1 = snd ((<$>) r1) in
      let s2 = snd ((<$>) r2) in
        s1 + s2

public export
run_seq : Int -> Int
run_seq x = sumEuler x

public export
run_seq_dc : Int -> Int
run_seq_dc x = sumEulerDC (mkList x)

public export
run_examples : Int -> Int -> Int -> Int
run_examples nw size t = sumEulerDCP (t, mkList size)
