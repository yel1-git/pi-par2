module ParCpi

import Data.List
import Data.Vect
import Pipar2

-- simple sequential map reduce (from Eden)
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

