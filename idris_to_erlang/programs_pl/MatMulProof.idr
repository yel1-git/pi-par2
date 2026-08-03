module MatMulProof

import Data.List
import Data.Vect
import Pipar2
import ProofLib
import ParMatMul

-- mapping g over an append is the same as 
-- mapping g over xs and then appending that to mapping g over ys
public export
mapAppendHom : (g : a -> b) -> (xs, ys : List a)
            -> mapImpl g (xs ++ ys) = mapImpl g xs ++ mapImpl g ys
mapAppendHom g [] ys = Refl
mapAppendHom g (x :: xs) ys = cong (g x ::) (mapAppendHom g xs ys)


public export
matMulWorkerHom : (matB : Vect n (Vect n Int))
               -> ListHom (Vect n Int) (List (Vect n Int))
                          (\chunk => computeChunk (chunk, matB))
                          (++) []
matMulWorkerHom matB = MkListHom (mapAppendHom (\row => multiply_row_by_col row matB)) Refl

public export
matMulChunkingSound : (matB : Vect n (Vect n Int)) 
                  -> (k : Nat) 
                  -> (xs : List (Vect n Int))
                  -> foldr (++) [] (syncPList (toPListWithChunk (\chunk => computeChunk (chunk, matB)) (S k) xs))
                       = computeChunk (xs, matB)
matMulChunkingSound matB k xs = chunkingSound (matMulWorkerHom matB) k xs


public export
parMatMulSound : (n : Nat) 
               -> (k : Nat) 
               -> (matA : List (Vect n Int)) 
               -> (matB : Vect n (Vect n Int))
               -> parMatMul (S k) n matA matB = computeChunk (matA, transpose1 n matB)
parMatMulSound n k matA matB =
   let p1 = matMulChunkingSound (transpose1 n matB) k matA
       -- parMatMul = concat (syncPList (toPListWithChunk f chunkSize matA))
       -- l;ambda defined in parMatMul
       p2 = concatEqualsFoldrAppend (syncPList (toPListWithChunk (\chunk => computeChunk (chunk, (transpose1 n matB))) (S k) matA))
   in trans p2 p1 

