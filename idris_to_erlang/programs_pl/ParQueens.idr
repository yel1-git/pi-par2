module ParQueens

import Data.List
import Pipar2

public export
check : (Int, Int) -> (Int, Int) -> Bool
check (c, l) (i, j) = (l == j) || ((c + l) == (i + j)) || ((c - l) == (i - j))

public export
safe : List Int -> Int -> Bool
safe p n =
  let indices = zip (map cast [1..length p]) p
  in foldr (\x, y => x && y) True (map (\(i, j) => not (check (i, j) (cast (length p) + 1, n))) indices)

public export
rainhas2 : Int -> Int -> Int -> List (List Int)
rainhas2 0 linha numero = [[]]
rainhas2 m linha numero =
  let cols = map cast [linha..numero] ++ map cast [1..linha-1]
      ps = rainhas2 (m - 1) linha numero
  in concatMap (\p =>
    map (\n => p ++ [n]) (filter (\n => safe p n) cols)
    ) ps

public export
prainhas : Int -> Int -> List (List Int)
prainhas numero linha = rainhas2 numero linha numero

public export
search : Int -> Int -> List (List Int)
search numero n =
  let all = prainhas numero n
  in filter (\a => case a of (x :: _) => x == n; _ => False) all

public export
rainhas : Int -> List (List Int)
rainhas n = concatMap (\x => search n x) [1..n]

public export
mkMsg : Int -> List Int -> List (Int, Int)
mkMsg s [] = []
mkMsg s (x :: xs) = (s, x) :: mkMsg s xs

public export
computeChunk : List (Int, Int) -> List (List Int)
computeChunk chunk = concatMap (uncurry search) chunk

public export
parQueens : Nat -> List (Int, Int) -> List (List Int)
parQueens k input =
  let plist = toPListWithChunk computeChunk k input
  in let synced = syncPList plist
     in concat synced

run : Nat -> Int -> List (List Int)
run nw size =
  let input = mkMsg (cast size) (map cast [1..size])
  in parQueens nw input
