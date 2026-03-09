module Queens2

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
run : Int -> List (List Int)
run n = rainhas n
