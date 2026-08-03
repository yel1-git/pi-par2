module Proofs.SumEulerProof

import Data.List
import Pipar2
import Proofs.ProofLib
import ParSumEuler2

-- Int addition treated as an idealised monoid. Unlike CpiProof's
-- doubleAddMonoid, this states a TRUE fact -- Int addition really is
-- associative -- Idris simply cannot derive it structurally, since `Int`
-- (unlike `Nat`) isn't an inductively-defined type. Same idiom as CpiProof:
-- Idris2 has no `postulate` keyword, so believe_me is the axiom.
public export
intAddMonoid : Monoid Int (+) 0
intAddMonoid = MkMonoid (\x, y, z => believe_me ()) (\x => believe_me ()) (\x => believe_me ())

-- computeChunk's `sum` goes through Prelude's List-specialised, foldl-based
-- foldMap (sum = concat @{Additive} = foldMap id @{Additive} = foldl (+) 0),
-- so `computeChunk chunk` is definitionally `foldl (+) 0 (map euler chunk)`
-- -- folding over the ALREADY-MAPPED list, unlike Queens' concatMap (which
-- fuses the mapping into the fold step directly). So this mirror needs one
-- more step than Queens': foldlMapFusionGen bridges "fold over mapped list"
-- to "fold with h fused per-step", THEN foldlFoldrAgree bridges foldl to
-- foldr, THEN foldrMapAgree bridges back to a foldr-over-mapped-list form.
public export
computeChunkMirror : (chunk : List Int)
                  -> ParSumEuler2.computeChunk chunk = foldr (+) 0 (mapImpl ParSumEuler2.euler chunk)
computeChunkMirror chunk =
  let p1 = foldlMapFusionGen (+) ParSumEuler2.euler 0 chunk
      p2 = foldlFoldrAgree intAddMonoid ParSumEuler2.euler chunk
      p3 = foldrMapAgree ParSumEuler2.euler chunk
  in trans p1 (trans p2 p3)

public export
sumEulerWorkerHom : ListHom Int Int ParSumEuler2.computeChunk (+) 0
sumEulerWorkerHom = MkListHom homApp homNilE
  where
    mirrorHom : ListHom Int Int (\xs => foldr (+) 0 (mapImpl ParSumEuler2.euler xs)) (+) 0
    mirrorHom = foldMapHom intAddMonoid ParSumEuler2.euler

    homApp : (xs, ys : List Int)
          -> ParSumEuler2.computeChunk (xs ++ ys) = ParSumEuler2.computeChunk xs + ParSumEuler2.computeChunk ys
    homApp xs ys =
      let p1 = computeChunkMirror (xs ++ ys)
          p2 = homAppendProj mirrorHom xs ys
          p3 = sym (computeChunkMirror xs)
          p4 = sym (computeChunkMirror ys)
          p5 = cong2 (+) p3 p4
          p6 = trans p2 p5
      in trans p1 p6

    homNilE : ParSumEuler2.computeChunk [] = 0
    homNilE =
      let p1 = computeChunkMirror []
          p2 = homNilProj mirrorHom
      in trans p1 p2

--------------------------------------------------------------------------
-- The theorem: chunking SumEuler's real per-chunk worker via
-- toPListWithChunk/syncPList, at ANY chunk size, reproduces exactly the
-- flat (unchunked) computeChunk result -- chunkingSound (ProofLib)
-- specialised to SumEuler's literal worker function.
--------------------------------------------------------------------------

public export
sumEulerChunkingSound : (k : Nat) -> (xs : List Int)
                     -> foldr (+) 0 (syncPList (toPListWithChunk ParSumEuler2.computeChunk (S k) xs)) = ParSumEuler2.computeChunk xs
sumEulerChunkingSound k xs = chunkingSound sumEulerWorkerHom k xs

--------------------------------------------------------------------------
-- Closing the gap to parSumEuler itself: like parCpi (and unlike
-- parMatMul/parQueens), its outer combine is already `foldr (+) 0 synced`,
-- not `concat`, so there is no concatEqualsFoldrAppend bridging step needed.
--------------------------------------------------------------------------

public export
parSumEulerSound : (k : Nat) -> (input : List Int)
                -> ParSumEuler2.parSumEuler (S k) input = ParSumEuler2.computeChunk input
parSumEulerSound k input = sumEulerChunkingSound k input
