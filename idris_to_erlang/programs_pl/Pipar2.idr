module Pipar2

import Data.List
import Data.Vect

public export
data PKind :  Type where
  Su : Nat -> PKind -- suspended (with no. element available)
  O  : PKind -- open (not yet applied)

-- e.g. Proc (a -> b) o 
public export
data Proc : Type -> PKind -> Type where

public export
proc : (a -> b) -> Proc (a -> b) O -- primitive

public export
procN : (a -> b) -> (n : Nat) -> Vect n (Proc (a -> b) O) -- derived

-- apply

-- apply function to a process
infixr 4 <#$>
public export
(<#$>) : Proc a (Su n) -> (f : a -> b) -> Proc b (Su n)

-- fold in a process
infixr 4 <#++>
public export
(<#++>) : Proc a (Su n) -> (f : a -> a -> a) -> a -> Proc a (Su 1)

infixr 4 <#$$>
public export
(<#$$>) : (f : a -> b -> c)
       -> Proc a (Su n)
       -> Proc b (Su n)
       -> Proc c (Su n)

infixr 4 <#>
public export
(<#>) : Proc (a -> b) O -> a -> Proc b (Su 1) 


infixr 4 <##>
public export
(<##>) :    Proc (a -> b) O 
         -> Vect n a 
         -> Proc b (Su n) -- distributeL


infixr 4 <###>
public export
(<###>) : Vect n (Proc (a -> b) O) 
       -> Vect n (Vect m a) 
       -> Vect n (Proc b (Su m))

infixr 4 <####>
public export
(<####>) : Vect n (Proc (a -> b) O) 
       -> Vect n a 
       -> Vect n (Proc b (Su 1))


-- auxilary chunk
public export
chunk2 : (Vect n a) -> (n : Nat) -> (m : Nat) 
     -> (prf : m `mod` n = 0) -> Vect n (Vect (m `div` n) a)

-- sync
infixr 4 <$>
public export
(<$>) : Proc b (Su (S k)) -> (Proc b (Su k), b) -- sync_stream


infixr 4 <$$>
public export
(<$$>) : Vect n (Proc b (Su m)) -> Vect (m*n) b -- derived


infixr 4 <$$>
public export
(<$$$>) : (Proc b (Su (S k))) -> Vect (S k) b -- derived


-- composition

infixr 4 <>>>
public export
(>>) : Proc (a -> b) O
    -> Proc (b -> c) O
    -> Proc (a -> c) O

-- skeletons
public export
farmS : {m : Nat}
     -> (f : a -> b) 
     -> (nw : Nat) 
     -> (xs : Vect m a)
     -> (prf : m `mod` nw = 0) 
     -> Vect nw (Proc b (Su (m `div` nw)))
farmS {m} f nw xs prf = 
    let ps = procN f nw 
        ch = chunk2 xs nw m prf
        qs = ps <###> ch
    in qs

divLem : {m : Nat} -> {nw : Nat} 
      -> Vect (mult (divNat m nw) nw) a -> Vect m a 
-- needs to be proved

public export
farm : {m : Nat}
    -> (f : a -> b) 
    -> (nw : Nat) 
    -> (xs : Vect m a) 
    -> (prf : m `mod` nw = 0) 
    -> Vect m b
farm f nw xs prf = 
    let r = farmS f nw xs prf in 
        let rr = (<$$>) r in divLem rr 
    
farmSHelper : {nw : Nat} 
           -> {m : Nat}
           -> {b : Type}
           -> Vect nw (Proc b (Su m))
          ->  Vect (mult m nw) b 
farmSHelper {nw} {m} {b} xs = (<$$>) xs

public export
farmS' : {m : Nat}
      -> {b : Type} 
      -> (f : a -> b) 
      -> (nw : Nat) 
      -> (xs : Vect m a) 
      -> (prf : m `mod` nw = 0) 
      -> Proc b (Su m)
farmS' {m} {b} f nw xs prf = 
  let ys = farmS f nw xs prf 
      ysA = (<$$>) ys -- ?pp -- ?oo -- (\zs => (<$$>) zs) -- zs)
      ysA' = divLem ysA
      p    = proc (\x => x)
      r    = p <##> ysA'
  in r

data Stages : (n : Nat) -> (a : Type) -> (b : Type) -> (c : Type) -> Type where
  MkStagesNil : Stages Z a a a  
  MkStages : (s1 : Proc (b -> c) O)
          -> (ss : Stages n a d b)
          -> Stages (S n) a b c

foldStages : (ss : Stages (S n) a b d) -> Proc (a -> d) O
foldStages (MkStages s2 MkStagesNil) = s2
foldStages (MkStages s2 (MkStages s3 ss2)) =  
    case foldStages (MkStages s3 ss2) of 
        p2 => p2 >> s2

-- mkStages : (s : Vect n t)
--        -> ( (a,b,z) ** Stages n a b z)
 
pipeS : (stages : Stages n a b z)
     -> (input : Vect (S m) a)
     -> Maybe (Proc z (Su (S m)))
pipeS MkStagesNil input = Nothing
pipeS (MkStages s1 ss) input = 
    Just ((foldStages (MkStages s1 ss)) <##> input)

pipe : (stages : Stages n a b z)
    -> (input : Vect (S m) a)
    -> Maybe (Vect (S m) z)
pipe stages input = case pipeS stages input of 
                        Nothing => Nothing 
                        Just ps => Just (Pipar2.(<$$$>) ps)

vectLem1 : (n : Nat) -> Vect (plus n (mult 0 n)) b -> Vect n b 
vectLem1 Z [] = []
vectLem1 (S n) (x::xs) = let hyp = vectLem1 n xs in x::hyp

dc :   (split : a -> (n ** Vect n a))
   ->  (join : {n : Nat} -> Vect n b -> b)
   ->  (thres : a -> Bool)
   ->  (solve : a -> b)
   ->  (input : a )
   ->  b
dc split join thre solve input with (thre input)
    dc split join thre solve input | True = solve input
    dc split join thre solve input | False =
        let (n ** xs) = split input
            re = procN (dc split join thre solve) n
            reA = re <####> xs
            syn = vectLem1 n ((<$$>) reA)
        in join {n} syn

-- basic implementation of a parallel list.
-- the cons constructor enforces that the head
-- is in a process.

public export
data ChkKind : Type where 
  Flat : ChkKind

  -- each chunk is a different size
  -- TODO!
  {-
  ChkHet  : (n : Nat)
         -> (rest : List Nat)
         -> ChkKind

  -}

  -- each chunk is the same size
  ChkHom  : (n : Nat)
         -> ChkKind
         
  -- chunk in two dimensions
  ChkMat  : (n : Nat) -> (m : Nat) -> ChkKind

public export
data PList : (a : Type)
          -> (chkd : ChkKind) 
      --    -> (i : Nat)     -- size of inner chunk
      --    -> (o : Nat)    -- size of outer chunk
          -> Type where
    PNil  : PList a Flat -- 0 0

    -- PNilChkHet : PList a (ChkHet 0 [])

    PCons :  (hd : Proc a (Su 1))
          -> (tl : PList a Flat)
          -> PList a Flat

    -- not sure we need this case at all...
    -- if we chunk then can simply cons a process with List a as the head...
    -- PConsChkHet  :  (hd : Proc a (Su (S n)))
    --             -> (tl : PList a (ChkHet t ts))
    --             -> PList a (ChkHet (S n) (t :: ts))

    PConsChkHom : (hd : Proc a (Su (S n)))
               -> (tl : PList a (ChkHom (S n)))
               -> PList a (ChkHom (S n))

    PNilChkHom : PList a (ChkHom (S n))

-- split list into N chunks
public export
splitChunk : Nat -> List a -> List(List a)
splitChunk k xs = case splitAt k xs of
      (chunk, []) => [chunk]
      (chunk, rest) => chunk :: splitChunk k rest

-- create PList with chunks
-- each chunk is processed by a separate worker
public export
toPListWithChunk : (List a -> b) -> Nat -> List a -> PList b Flat
toPListWithChunk f k xs = case splitChunk k xs of
  Nil => PNil
  (chunk :: chunks) =>
    let procChunk = proc (\y => y) <#> f chunk
    in PCons procChunk (toPListWithChunk f k (concat chunks))

public export
syncPList : PList a chkd -> List a
syncPList PNil = Nil
syncPList (PCons p rest) = case (<$>) p of (_, v) => v :: syncPList rest
syncPList PNilChkHom = Nil
syncPList (PConsChkHom p rest) = case (<$>) p of (_, v) => v :: syncPList rest

