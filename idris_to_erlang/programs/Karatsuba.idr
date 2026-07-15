module Karatsuba

import Data.List
import Data.Nat

-- Sequential divide-and-conquer skeleton.
public export
dc_rw : Nat
     -> (trivial : a -> Bool)
     -> (solve : a -> b)
     -> (split : a -> List a)
     -> (combine : a -> List b -> b)
     -> a
     -> b
dc_rw Z     trivial solve split combine input = solve input
dc_rw (S d) trivial solve split combine input =
  if trivial input
    then solve input
    else combine input (map (dc_rw d trivial solve split combine) (split input))

public export
LongInteger : Type
LongInteger = List Int

-- pad on the left (the most-significant end) so two big-endian
-- vectors of different lengths still line up at their least
-- significant (rightmost) digit once zipped.
padTo : Nat -> LongInteger -> LongInteger
padTo n xs = replicate (n `minus` length xs) 0 ++ xs

zipWithPad : (Int -> Int -> Int) -> LongInteger -> LongInteger -> LongInteger
zipWithPad f xs ys =
  let n = max (length xs) (length ys)
  in zipWith f (padTo n xs) (padTo n ys)

addLI : LongInteger -> LongInteger -> LongInteger
addLI = zipWithPad (+)

subLI : LongInteger -> LongInteger -> LongInteger
subLI = zipWithPad (-)

-- multiply by B^m: append m trailing (least-significant) zero digits
shiftLI : Nat -> LongInteger -> LongInteger
shiftLI m xs = xs ++ replicate m 0

trivial : (LongInteger, LongInteger) -> Bool
trivial (i1, i2) = length i1 <= 1 || length i2 <= 1

solve : (LongInteger, LongInteger) -> LongInteger
solve ([], _)  = []
solve (_, [])  = []
solve ([a], y) = map (a *) y
solve (x, [b]) = map (* b) x
solve _        = [] -- unreachable

split : (LongInteger, LongInteger) -> List (LongInteger, LongInteger)
split (x, y) =
  let m       = max (length x) (length y) `div` 2
      (x1,x0) = splitAt (length x `minus` m) x
      (y1,y0) = splitAt (length y `minus` m) y
  in [(x0, y0), (x1, y1), (addLI x0 x1, addLI y0 y1)]

combine : (LongInteger, LongInteger) -> List LongInteger -> LongInteger
combine (x, y) [z0, z2, z1raw] =
  let m  = max (length x) (length y) `div` 2
      z1 = subLI (subLI z1raw z2) z0
  in addLI (addLI z0 (shiftLI m z1)) (shiftLI (2 * m) z2)
combine _ _ = [] -- unreachable

depth : LongInteger -> LongInteger -> Nat
depth i1 i2 = length i1 + length i2 + 1

export
karat : LongInteger -> LongInteger -> LongInteger
karat i1 i2 = dc_rw (depth i1 i2) trivial solve split combine (i1, i2)

-- Evaluate a coefficient vector as a base-10
-- integer, with the most-significant digit first (Horner's
-- method: read left to right. Big-Endian.
export
evalLI : LongInteger -> Integer
evalLI = foldl (\acc, d => acc * 10 + cast d) 0

mkList : Nat -> List Int
mkList n = replicate n 7

-- example run. 
main : IO ()
main = printLn (length (karat (mkList 90000) (mkList 90000)))