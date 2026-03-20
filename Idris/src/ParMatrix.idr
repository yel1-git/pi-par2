module ParMatrix 

import Pipar2
import Data.List
import Data.Nat
import Data.Vect

data ParMatrix : (a    : Type)
             ->  (cols : Nat)    -- row dimension
             ->  (rows : Nat)    -- col dimension
             ->  (chkd : ChkKind)
             -> Type where 
   PMNil : ParMatrix a 0 0 Flat 

   PMCons : (hd : Vect cols a)
        ->  (tl : ParMatrix a cols rows Flat)
        ->  ParMatrix a cols (S rows) Flat 

   
   PMNilChk : ParMatrix a (Chkmat (S n) (S m))

   PMConsChk : 
    