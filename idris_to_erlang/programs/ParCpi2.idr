import Pipar2 
import Data.List
import Data.Nat
import Data.Vect

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

parMapFol : -- (n : Nat) 
    (f : Proc a (Su (S n)) -> Proc b (Su 1)) 
 -> PList a (ChkHom (S n))
 -> PList b Flat
-- 1 parMapFol f PNil             impossible 
-- 2 parMapFol f (PCons hd tl)    impossible 
-- parMapFol f PNilChkHet       impossible
-- parMapFol f (PConsChkHet hd tl) impossible
parMapFol f PNilChkHom = PNil
parMapFol f (PConsChkHom hd tl) = let r = f hd 
                                      t = parMapFol f tl
                                  in PCons r t



foldr2 : (a -> a -> a) -> a 
    -> PList a chks  
    -> Proc a (Su 1) 
foldr2 f a PNil = (proc (\x => x) <#> a)
foldr2 f a (PCons hd tl) = let r = (foldr2 f a tl)
                            in (<#$$>) f hd r
foldr2 f a PNilChkHom = (proc (\x => x) <#> a)
foldr2 f a (PConsChkHom hd tl) = let hd' = (<$$$>) hd
                                     r   = (proc (\x => x) <#> foldr f a hd')
                                     res = foldr2 f a tl 
                                     u   = (<#$$>) f r res
                                 in u

mapRedr3 : (g : b -> b -> b) 
  -> (e : b)
  -> (f : a -> b) 
  -> (n : Nat)
  -> Proc a (Su n) 
  -> Proc b (Su 1)
mapRedr3 g e f n l = (<#++>) (l <#$> f) g e


vectToPList : (i : Vect m (Vect (S n) a)) -> PList a (ChkHom (S n))
vectToPList [] = PNilChkHom 
vectToPList (x::xs) =  let pr = proc (\x => x)
                           pr2 = pr <##> x
                       in     
                       PConsChkHom pr2 (vectToPList xs)

parMapRedr2 : (n : Nat) -> (g : b -> b -> b) -> (e : b) 
    -> (f : a -> b) 
    -> (i : PList a (ChkHom (S n)))
    -> Proc b (Su 1)
parMapRedr2 n g e f i = 
 let -- s  = splitIntoN2 n i -- PList a (Chk x y)
     f' = mapRedr3 g e f (S n)-- Plist a Flat -> Proc b (Su 1)
     ma = parMapFol f'  i -- PList a ?chk -> PList (Proc b (Su 1)) ?chk
     fo = foldr2 g e ma  -- PList b ?chks -> Proc b (Su 1)
 in fo

f : Double -> Double
f x = 4 / (1 + x * x)
 
index : Integer -> Double
index i = cast i - 0.5
 
index2 : Integer -> Integer -> Double
index2 i n = index i / cast n


parCpi : (nw : Nat)
      -> (n : Nat) 
      -> (n2 : Integer) 
      -> (prf : modNat (S nw) n = 0)
      -> (i : Vect n Integer) -> Double
parCpi (S nw) n n2 prf v =  
                let i = chunk3 v n (S nw) prf 
                    r = (<$>) (parMapRedr2 ((divNat (S nw) n)) (+) 0 (\ind => f (index2 ind (fromInteger n2))) (vectToPList i))
                in (snd r) / cast n2
parCpi Z _ _ _ _ = 0.0


{-
parCpi : (chunkSize : Nat) -> (totalN : Integer)
      -> PList Integer chks -> Proc Double (Su 1)
parCpi chunkSize totalN input =
  parMapRedr2 chunkSize (+) 0.0
    (\i => let x = (fromInteger i - 0.5) / fromInteger totalN
           in 4.0 / (1.0 + x * x) / fromInteger totalN)
    input
-}