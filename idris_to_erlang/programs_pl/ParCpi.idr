module ParCpi

import Data.List
import Data.Vect
import Pipar2

mapRedr : (b -> c -> c) -> c -> (a -> b) -> List a -> c
mapRedr g e f xs = foldr g e (map f xs)

f : Double -> Double
f x = 4 / (1 + x * x)

index : Integer -> Double
index i = cast i - 0.5

index2 : Integer -> Integer -> Double
index2 i n = index i / cast n

cpi : Integer -> Double
cpi n = mapRedr (+) 0 (\i => f (index2 i n)) [1..n] / cast n

computeChunk : List Integer -> Integer -> Double
computeChunk Nil n = 0
computeChunk (i :: is) n =
  let x = (cast i - 0.5) / cast n in
  let val = 4.0 / (1.0 + x * x) / cast n in
  val + computeChunk is n

parCpi : Nat -> Integer -> List Integer -> Double
parCpi k n xs =
  let f = \chunk => computeChunk chunk n
  in let plist = toPListWithChunk f k xs
  in let synced = syncPList plist
  in foldr (+) 0 synced
