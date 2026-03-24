module ParMatMul

import Data.Vect
import Data.Fin
import Decidable.Equality
import Pipar2

public export
transpose1 : (n : Nat) -> Vect m (Vect n a) -> Vect n (Vect m a)
transpose1 0     _         = []
transpose1 n     []        = replicate n []
transpose1 (S k) (x :: xs) = map head (x :: xs) :: transpose1 k (map tail (x :: xs))

public export
dot_product : Vect n Int -> Vect n Int -> Int
dot_product [] [] = 0
dot_product (a :: as) (b :: bs) = a * b + dot_product as bs

public export
multiply_row_by_col : Vect n Int -> Vect m (Vect n Int) -> Vect m Int
multiply_row_by_col row [] = []
multiply_row_by_col row (col :: cols) =
  dot_product row col :: multiply_row_by_col row cols

public export
multiply_internal : Vect m (Vect n Int) -> Vect n (Vect n Int) -> Vect m (Vect n Int)
multiply_internal [] b = []
multiply_internal (row :: rows) b =
  multiply_row_by_col row b :: multiply_internal rows b

public export
multiply : (n : Nat) -> Vect m (Vect n Int) -> Vect n (Vect n Int) -> Vect m (Vect n Int)
multiply n a b = multiply_internal a (transpose1 n b)

public export
rangeFrom : Int -> (n : Nat) -> Vect n Int
rangeFrom start 0     = []
rangeFrom start (S k) = start :: rangeFrom (start + 1) k

computeChunk : (List (Vect n Int), Vect n (Vect n Int)) -> List (Vect n Int)
computeChunk (chunk, matB) = map (\row => multiply_row_by_col row matB) chunk

parMatMul : (chunkSize : Nat) -> (n : Nat) -> (matA : List (Vect n Int)) -> (matB : Vect n (Vect n Int)) -> List (Vect n Int)
parMatMul chunkSize n matA matB =
  let transposedB = transpose1 n matB
  in let f = \chunk => computeChunk (chunk, transposedB)
     in let plist = toPListWithChunk f chunkSize matA
        in let synced = syncPList plist
           in concat synced

run : (nw : Nat) -> (size : Nat) -> List (Vect size Int)
run nw size =
  let row = rangeFrom 1 size
      matA = replicate size row
      matB = replicate size row  
  in parMatMul nw size (vectToList matA) matB
  where
    vectToList : Vect n a -> List a
    vectToList [] = Nil
    vectToList (x :: xs) = x :: vectToList xs
