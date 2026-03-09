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
red : (Int, Int) -> Int -> Int
red (x, y) sum = x * y + sum

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
multiply : (n : Nat)  -> Vect m (Vect n Int) -> Vect n (Vect n Int) -> Vect m (Vect n Int)
multiply n a b = multiply_internal a (transpose1 n b)

public export
mkMsg : Vect n a -> Vect n a
mkMsg [] = []
mkMsg (x :: xs) = x :: mkMsg xs

public export
rangeFrom : Int -> (n : Nat) -> Vect n Int
rangeFrom start 0     = []
rangeFrom start (S k) = start :: rangeFrom (start + 1) k

-- farmIt with runtime proof — m explicit for transpiler
farmIt : (m : Nat) -> (nw : Nat) -> Vect m (Vect m Int) -> Vect m (Vect m Int) -> Vect m (Vect m Int)
farmIt m nw matA matB =
  case decEq (m `mod` nw) 0 of
    Yes prf => farm (\row => multiply_row_by_col row matB) nw matA prf
    No _    => matA

-- public export
-- farmIt : {m : Nat} -> Nat -> Vect m (Vect m Int) -> Vect m (Vect m Int) -> Vect m (Vect m Int)
-- farmIt nw matA matB = farm (\row => multiply_row_by_col row matB) nw matA (believe_me ())

public export
run : (nw : Nat) -> (size : Nat) -> Vect size (Vect size Int)
run nw size =
  let row = rangeFrom 1 size
      matA = replicate size row
      matB = transpose1 size matA
  in farmIt size nw matA matB
