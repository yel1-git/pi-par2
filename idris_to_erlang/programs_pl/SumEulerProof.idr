module SumEulerProof

import Data.List
import Data.Nat
import Pipar2
import ProofLib
import ParSumEuler2

public export
eulerN : Int -> Nat
eulerN n = length (filter (\x => relPrime n x) (mkList n))

public export
computeChunkN : List Int -> Nat
computeChunkN = \xs => foldr (+) 0 (map SumEulerProof.eulerN xs)

public export
natAddMonoid : Monoid Nat (+) 0
natAddMonoid = MkMonoid plusAssociative plusZeroLeftNeutral plusZeroRightNeutral

public export
sumEulerHom : ListHom Int Nat (\xs => foldr (+) 0 (map SumEulerProof.eulerN xs)) (+) 0
sumEulerHom = foldMapHom natAddMonoid SumEulerProof.eulerN

public export
sumEulerChunkingSound : (k : Nat) -> (xs : List Int)
                      -> foldr (+) (the Nat 0) (syncPList (toPListWithChunk (\xs => foldr (+) 0 (map SumEulerProof.eulerN xs)) (S k) xs))
                         = foldr (+) 0 (map SumEulerProof.eulerN xs)
sumEulerChunkingSound k xs = chunkingSound sumEulerHom k xs

