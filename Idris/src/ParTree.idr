module ParTree

import Pipar2 
import Data.List
import Data.Nat
import Data.Vect

data PBinTree : (a : Type) 
             -> (Chkd : ChkKind)
             -> Type where

    PTNil : PBinTree a Flat 

    PNode : (lf : Proc a (Su 1))
         -> (tl : PBinTree a Flat)
         -> (tr : PBinTree a Flat)
         -> PBinTree a Flat

    PTNilChk : PBinTree a (ChkHom (S n))

    PNodeParChk : (lf : Proc a (Su 1))
               -> (tl : Proc (tree a)  (Su 1))
               -> (tr : Proc (tree a ) (Su 1))
               -> PBinTree a (ChkHom 0)

    PNodeParNotChk : (lf : Proc a (Su 1))
                  -> (tl : PBinTree a (ChkHom n))
                  -> (tr : PBinTree a (ChkHom n))
                  -> PBinTree a (ChkHom (S n))

       
    

parMapTree : (f : a -> b) 
          -> (PBinTree a chks)
          -> (PBinTree b chks)
parMapTree f PTNil = PTNil 
parMapTree f (PNode lf tl tr) = PNode (lf <#$> f) 
                                      (parMapTree f tl)
                                      (parMapTree f tr)
