module QueensProof

import Data.List
import Pipar2
import ProofLib
import ParQueens


-- foldr (\x,acc => h x ++ acc) [] xs and foldr (++) [] (map h xs) compute
-- the same thing

public export
foldrMapFoldr : (h : a -> List b) -> (xs : List a)
             -> foldr (\x, acc => h x ++ acc) [] xs = foldr (++) [] (mapImpl h xs)
foldrMapFoldr h [] = Refl
foldrMapFoldr h (x :: xs) = cong (h x ++) (foldrMapFoldr h xs)


public export
computeChunkMirror : (chunk : List (Int, Int))
                  -> ParQueens.computeChunk chunk = foldr (++) [] (mapImpl (uncurry ParQueens.search) chunk)
computeChunkMirror chunk =
  let p1 = foldlFoldrAgree listAppendMonoid (uncurry ParQueens.search) chunk
      p2 = foldrMapFoldr (uncurry ParQueens.search) chunk
  in trans p1 p2


public export
queensWorkerHom : ListHom (Int, Int) (List (List Int)) ParQueens.computeChunk (++) []
queensWorkerHom = MkListHom homApp homNilQ
  where
    mirrorHom : ListHom (Int, Int) (List (List Int)) (\xs => foldr (++) [] (mapImpl (uncurry ParQueens.search) xs)) (++) []
    mirrorHom = foldMapHom listAppendMonoid (uncurry ParQueens.search)

    homApp : (xs, ys : List (Int, Int)) -> ParQueens.computeChunk (xs ++ ys) = ParQueens.computeChunk xs ++ ParQueens.computeChunk ys
    homApp xs ys =
      let p1 = computeChunkMirror (xs ++ ys)
          p2 = homAppendProj mirrorHom xs ys
          p3 = sym (computeChunkMirror xs)
          p4 = sym (computeChunkMirror ys)
          p5 = cong2 (++) p3 p4
          p6 = trans p2 p5
      in trans p1 p6

    homNilQ : ParQueens.computeChunk [] = []
    homNilQ =
      let p1 = computeChunkMirror []
          p2 = homNilProj mirrorHom
      in trans p1 p2



public export
queensChunkingSound : (k : Nat) -> (xs : List (Int, Int))
                    -> foldr (++) [] (syncPList (toPListWithChunk ParQueens.computeChunk (S k) xs)) = ParQueens.computeChunk xs
queensChunkingSound k xs = chunkingSound queensWorkerHom k xs



public export
parQueensSound : (k : Nat) -> (input : List (Int, Int))
              -> ParQueens.parQueens (S k) input = ParQueens.computeChunk input
parQueensSound k input =
  let p1 = queensChunkingSound k input
      p2 = concatEqualsFoldrAppend (syncPList (toPListWithChunk ParQueens.computeChunk (S k) input))
  in trans p2 p1
