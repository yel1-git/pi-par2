module Proofs.CpiProof

import Data.List
import Pipar2
import Proofs.ProofLib
import ParCpi

public export
cpiTerm : Integer -> Integer -> Double
cpiTerm n i =
  let x = (cast i - 0.5) / cast n
  in 4.0 / (1.0 + x * x) / cast n

public export
computeChunkMirror : (n : Integer) -> (chunk : List Integer)
                  -> ParCpi.computeChunk chunk n = foldr (+) 0 (mapImpl (cpiTerm n) chunk)
computeChunkMirror n [] = Refl
computeChunkMirror n (i :: is) =
  let indHyp = computeChunkMirror n is
  in cong (cpiTerm n i +) indHyp

-- IEEE-754 Double
-- addition is NOT associative under rounding.
-- we assume associativity holds for (+) over doubles here. 
-- make this an assumption.
public export
doubleAddMonoid : Monoid Double (+) 0
doubleAddMonoid = MkMonoid (\x, y, z => believe_me ()) (\x => believe_me ()) (\x => believe_me ())

public export
cpiWorkerHom : (n : Integer) -> ListHom Integer Double (\chunk => ParCpi.computeChunk chunk n) (+) 0
cpiWorkerHom n = MkListHom homApp homNilC
  where
    mirrorHom : ListHom Integer Double (\xs => foldr (+) 0 (mapImpl (cpiTerm n) xs)) (+) 0
    mirrorHom = foldMapHom doubleAddMonoid (cpiTerm n)

    homApp : (xs, ys : List Integer)
          -> ParCpi.computeChunk (xs ++ ys) n = ParCpi.computeChunk xs n + ParCpi.computeChunk ys n
    homApp xs ys =
      let p1 = computeChunkMirror n (xs ++ ys)
          p2 = homAppendProj mirrorHom xs ys
          p3 = sym (computeChunkMirror n xs)
          p4 = sym (computeChunkMirror n ys)
          p5 = cong2 (+) p3 p4
          p6 = trans p2 p5
      in trans p1 p6

    homNilC : ParCpi.computeChunk [] n = 0
    homNilC =
      let p1 = computeChunkMirror n []
          p2 = homNilProj mirrorHom
      in trans p1 p2

public export
cpiChunkingSound : (n : Integer) -> (k : Nat) -> (xs : List Integer)
                -> foldr (+) 0 (syncPList (toPListWithChunk (\chunk => ParCpi.computeChunk chunk n) (S k) xs))
                     = ParCpi.computeChunk xs n
cpiChunkingSound n k xs = chunkingSound (cpiWorkerHom n) k xs

public export
parCpiSound : (n : Integer) -> (k : Nat) -> (xs : List Integer)
           -> ParCpi.parCpi (S k) n xs = ParCpi.computeChunk xs n
parCpiSound n k xs = cpiChunkingSound n k xs
