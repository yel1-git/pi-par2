module Pipar2

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
