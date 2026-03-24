module ParSumEuler2

import Data.List
import Data.Vect
import Pipar2

public export
gcd2 : Int -> Int -> Int
gcd2 a 0 = a
gcd2 a b = gcd2 b (a `mod` b)

public export
relPrime : Int -> Int -> Bool
relPrime x y = gcd2 x y == 1

public export
mkList : Int -> List Int
mkList n = [1..n]

public export
euler : Int -> Int
euler n = cast (length (filter (\x => relPrime n x) (mkList n)))

public export
sumEuler : Int -> Int
sumEuler n = sum (map euler (mkList n))

computeChunk : List Int -> Int
computeChunk chunk = sum (map euler chunk)

parSumEuler : Nat -> List Int -> Int
parSumEuler k input =
  let plist = toPListWithChunk computeChunk k input
  in let synced = syncPList plist
     in foldr (+) 0 synced

run_examples : Int -> Int -> Int
run_examples nw size =
  let list = mkList size
  in parSumEuler (cast nw) list
