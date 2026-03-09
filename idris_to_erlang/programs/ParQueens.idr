module ParQueens

import Data.List
import Data.Vect
import Pipar2

%default total

public export
check : (Int, Int) -> (Int, Int) -> Bool
check (c, l) (i, j) = (l == j) || ((c + l) == (i + j)) || ((c - l) == (i - j))

public export
safe : List Int -> Int -> Bool
safe p n =
  let indices = zip (take (length p) (iterate (+1) 1)) p
  in foldr (\x, y => x && y) True (map (\(i, j) => not (check (i, j) (cast (length p) + 1, n))) indices)

public export
rainhas2 : Int -> Int -> Int -> List (List Int)
rainhas2 0 linha numero = [[]]
rainhas2 m linha numero =
  let cols = (take (cast (numero - linha + 1)) (iterate (+1) linha))
              ++ (take (cast (linha - 1)) (iterate (+1) 1))
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
  in takeWhile (\a => case a of (x :: _) => x == n; _ => False) all

public export
rainhas : Int -> List (List Int)
rainhas n = map (\x => search n x) (take (cast n) (iterate (+1) 1))

public export
mkMsg : Int -> List Int -> List (Int, Int)
mkMsg s [] = []
mkMsg s (x :: xs) = (s, x) :: mkMsg s xs

public export
farm4RR : Int -> Int -> List (Int, Int) -> List (List (List Int))
farm4RR nw size input =
  let p = proc (\(s, m) => search s m)
      rs = map (\x => p <#> x) input
  in map (<$>) rs

public export
run : Int -> Int -> List (List (List Int))
run nw size =
  let input = mkMsg size (take (cast size) (iterate (+1) 1))
  in farm4RR nw size input
