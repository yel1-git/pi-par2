module ProofLib

import Data.List
import Pipar2

public export 
data Monoid : (t : Type) -> (g : t -> t -> t) -> (ident : t) -> Type where
  MkMonoid : (assocPrf : (x, y, z : t) -> g x (g y z) = g (g x y) z)
          -> (idLPrf   : (x : t) -> g ident x = x)
          -> (idRPrf   : (x : t) -> g x ident = x)
          -> Monoid t g ident

-- If g is a monoid, then it implies g is associative...
public export
monoidImpAssoc : Monoid t g ident -> (x, y, z : t) -> g x (g y z) = g (g x y) z
monoidImpAssoc (MkMonoid a _ _) = a

-- if g is a monoid with identity ident, then implies ident is left identity
public export
monoidImpLeftId : Monoid t g ident -> (x : t) -> g ident x = x
monoidImpLeftId (MkMonoid _ l _) = l

-- if g is a monoid with identity ident, then implies ident is a right identity
public export
monoidImpRightId : Monoid t g ident -> (x : t) -> g x ident = x
monoidImpRightId (MkMonoid _ _ r) = r


public export
data ListHom :   (a : Type)         -- the type of the list elements 
              -> (t : Type)         -- the type of the function over the list
              -> (f : List a -> t) 
              -> (g : t -> t -> t) 
              -> (ident : t) 
              -> Type where
  MkListHom : (homAppend : (xs, ys : List a) -> f (xs ++ ys) = g (f xs) (f ys))
           -> (homNil    : f [] = ident)
           -> ListHom a t f g ident

-- projections for append
public export
homAppendProj : ListHom a t f g i -> (xs, ys : List a) -> f (xs ++ ys) = g (f xs) (f ys)
homAppendProj (MkListHom ha _) = ha

public export
homNilProj : ListHom a t f g ident -> f [] = ident
homNilProj (MkListHom _ hn) = hn

-- g is a monoid with identity i
-- given a monoid for g and identity i, 
-- give a proof that, for all functions h, 
-- that folding over a mapped (appended list) is the same as folding over both lists and joining them with g
-- example: 
-- foldr (+) 0 (xs ++ ys) == foldr (+) 0 xs + foldr (+) 0 ys 
-- needed to prove that sumEuler is a homomorphism
-- (foldr (+) 0 (map SumEulerProof.eulerN xs)) (+) 0)


public export
foldMapHom : {g : t -> t -> t}
          -> {ident : t}
          -> (monPrf : Monoid t g ident) 
          -> (h : a -> t)
          -> ListHom a t (\xs => foldr g ident (map h xs)) g ident
foldMapHom {g} {ident} monPrf h = MkListHom prf1 Refl
  where 
    prf1 : (xs : List a) -> (ys : List a) 
     -> foldr g ident (mapImpl h (xs ++ ys)) 
          = g (foldr g ident (mapImpl h xs)) (foldr g ident (mapImpl h ys))
    prf1 [] ys = let p1 = monoidImpLeftId monPrf (foldr g ident (mapImpl h ys))
                 in sym p1
    prf1 (x::xs) ys = let p1 = monoidImpAssoc monPrf (h x) (foldr g ident (mapImpl h xs)) (foldr g ident (mapImpl h ys))
                          hyp = prf1 xs ys 
                          cP = cong (g (h x)) hyp 
                      in trans cP p1

    

public export
foldMapConcat : ListHom a t f g ident
             -> (chunks : List (List a))
             -> foldr g ident (map f chunks) = f (listConcat chunks)
foldMapConcat homPrf [] = sym (homNilProj homPrf)
foldMapConcat homPrf (x::xs) =
     let p1 = sym (homAppendProj homPrf x (listConcat xs))
         indHyp = foldMapConcat homPrf xs
     in rewrite indHyp in p1


public export
listAppendMonoid : Monoid (List a) (++) []
listAppendMonoid = MkMonoid appendAssociative (\_ => Refl) appendNilRightNeutral


-- g/ident are declared explicitly here (not left to auto-binding) because a
-- `let` on the right-hand side referencing an auto-bound implicit fails with
-- "g is not accessible in this context" -- an Idris2 elaboration quirk
-- distinct from (but in the same family as) the `rewrite`-in-`where` gotcha
-- noted elsewhere in this file. Declaring them explicitly sidesteps it
-- without changing the implicit call convention at any call site.
public export
foldlGen : {a : Type} -> {t : Type} -> {g : t -> t -> t} -> {ident : t}
        -> Monoid t g ident -> (h : a -> t) -> (z : t) -> (xs : List a)
        -> foldl (\acc, x => g acc (h x)) z xs = g z (foldr (\x, acc => g (h x) acc) ident xs)
foldlGen mon h z [] = sym (monoidImpRightId mon z)
foldlGen mon h z (x :: xs) =
  let p1 = foldlGen mon h (g z (h x)) xs
      p2 = monoidImpAssoc mon z (h x) (foldr (\y, acc => g (h y) acc) ident xs)
  in trans p1 (sym p2)

public export
foldlFoldrAgree : {a : Type} -> {t : Type} -> {g : t -> t -> t} -> {ident : t}
               -> Monoid t g ident -> (h : a -> t) -> (xs : List a)
               -> foldl (\acc, x => g acc (h x)) ident xs = foldr (\x, acc => g (h x) acc) ident xs
foldlFoldrAgree mon h xs =
  let p1 = foldlGen mon h ident xs
      p2 = monoidImpLeftId mon (foldr (\x, acc => g (h x) acc) ident xs)
  in trans p1 p2


-- {a} declared explicitly (not auto-bound) -- foldlFoldrAgree now declares its
-- own implicits explicitly (see the comment above foldlGen), and an
-- auto-bound implicit at a call site isn't reliably resolved against an
-- explicitly-declared one on the callee: same family of gotcha, one call
-- removed.
public export
concatEqualsFoldrAppend : {a : Type} -> (xss : List (List a)) -> concat xss = foldr (++) [] xss
concatEqualsFoldrAppend xss = foldlFoldrAgree listAppendMonoid id xss



public export 
splitAtAppend : (n : Nat) 
             -> (xs : List a) 
             -> (ys, zs : List a) 
             -> (prf : splitAt n xs = (ys, zs)) 
             -> ys ++ zs = xs
splitAtAppend Z xs ys zs prf = let p1 = cong (uncurry (++)) prf  
                               in sym p1
splitAtAppend (S k) [] ys zs prf = let p1 = cong (uncurry (++)) prf
                                   in sym p1
splitAtAppend (S k) (x::xs) ys zs prf with (splitAt k xs) proof eq 
    splitAtAppend (S k) (x::xs) ys zs prf | (xs', ys') = 
        let indHyp = splitAtAppend k xs xs' ys' eq 
            p1 = cong (x :: ) indHyp 
            p2 = cong (uncurry (++)) prf
            p3 = sym (trans (sym p1) p2)
        in p3

public export 
concatSplitChunk : (k : Nat) 
                -> (xs : List a) 
                -> listConcat (splitChunk (S k) xs) = xs
concatSplitChunk k [] = Refl
concatSplitChunk k (x::xs) with (splitAt (S k) (x::xs)) proof eq 
        concatSplitChunk k (x::xs) | (xs', []) = 
            let p = splitAtAppend (S k) (x::xs) xs' [] eq
            in p
        concatSplitChunk k (x::xs) | (xs', (r::rst)) = 
            let indHyp = concatSplitChunk k (r::rst) -- need to expand pattern so it knows it is not empty
                indHyp2 = splitAtAppend (S k) (x::xs) xs' (r::rst) eq 
                p1 = (cong (xs' ++ ) indHyp)
                p2 = (trans p1 indHyp2)
            in p2
{-
-- split list into N chunks
public export
splitChunk : Nat -> List a -> List(List a)
splitChunk k [] = []
splitChunk k xs = case splitAt k xs of
      (chunk, []) => [chunk]
      (chunk, rest) => chunk :: splitChunk k rest

-- structural flatten, used instead of Prelude's Foldable-derived `concat`
-- (which goes via foldl/foldMap and is much harder to reason about equationally)
public export
listConcat : List (List a) -> List a
listConcat [] = []
listConcat (c :: cs) = c ++ listConcat cs
-}    

public export
procAssumption : (f : List a -> t) 
              -> (chunk : List a) 
              -> (tail : PList t Flat)
              -> syncPList (PCons (proc (\y => y) <#> f chunk) tail) = f chunk :: syncPList tail

public export
syncToPListWithChunk : (f : List a -> t) 
                    -> (k : Nat) 
                    -> (xs : List a)
                    -> syncPList (toPListWithChunk f (S k) xs) = map f (splitChunk (S k) xs)
syncToPListWithChunk f k [] = Refl
syncToPListWithChunk f k (x::xs) with (splitAt (S k) (x::xs)) proof eq 
    syncToPListWithChunk f k (x::xs) | (xs', [])
      = let inHyp1 = procAssumption f xs' PNil 
        in inHyp1
    syncToPListWithChunk f k (x::xs) | (xs', (r::rst)) 
      = let inHyp1 = procAssumption f xs' (toPListWithChunk f (S k) (listConcat (splitChunk (S k) (r::rst))))
            inHyp2 = syncToPListWithChunk f k (r::rst)
            inHyp3 = cong (\z => syncPList (toPListWithChunk f (S k) z)) (concatSplitChunk k (r::rst))
            p1 = cong (f xs' :: ) (trans inHyp3 inHyp2)
            p2 = trans inHyp1 p1
        in p2


{-
public export
listConcat : List (List a) -> List a
listConcat [] = []
listConcat (c :: cs) = c ++ listConcat cs

-- create PList with chunks
-- each chunk is processed by a separate worker
public export
toPListWithChunk : (List a -> b) -> Nat -> List a -> PList b Flat
toPListWithChunk f k [] = PNil
toPListWithChunk f k xs = case splitChunk k xs of
  Nil => PNil
  (chunk :: chunks) =>
    let procChunk = proc (\y => y) <#> f chunk
    in PCons procChunk (toPListWithChunk f k (listConcat chunks))

public export
syncPList : PList a chkd -> List a
syncPList PNil = Nil
syncPList (PCons p rest) = case (<$>) p of (_, v) => v :: syncPList rest
syncPList PNilChkHom = Nil
syncPList (PConsChkHom p rest) = case (<$>) p of (_, v) => v :: syncPList rest
-}

public export
chunkingSound : {f : List a -> t}
             -> ListHom a t f g ident
             -> (k : Nat) 
             -> (xs : List a)
             -> foldr g ident (syncPList (toPListWithChunk f (S k) xs)) = f xs
chunkingSound {f} hom k xs = 
    -- p1 : foldr g ident (mapImpl f (splitChunk (S k) xs)) = f (listConcat (splitChunk (S k) xs))
    -- need to show that listConcat (splitChunk (S k) xs) = xs?
    let p1 = foldMapConcat hom (splitChunk (S k) xs) -- toPList uses splitChunk? proof is S k
        --  p2 : listConcat (splitChunk (S (S k)) xs) = xs 
        p2 = concatSplitChunk k xs 
        -- trans : (0 _ : a = b) -> (0 _ : b = c) -> a = c
        -- cong p2 to get f xs
        --  p3 : f (listConcat (splitChunk (S (S k)) xs)) = f xs
        p3 = cong f p2
        -- p4 : foldr g ident (mapImpl f (splitChunk (S k) xs)) = f xs
        p4 = trans p1 p3 
        -- use syncToPListWith chunk  to get syncPList (toPListWithChunk f (S k) xs) = map f (splitChunk (S k) xs)
        p5 = cong (foldr g ident) (syncToPListWithChunk f k xs)
        p6 = trans p5 p4
    in p6