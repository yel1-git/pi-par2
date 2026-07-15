module Grobner

import Data.List
import Data.Maybe
import Data.Nat

-- Sequential port of the Gröbner-bases case study (section 5.3,
-- Figure 25) from "Parallel Functional Programming in Eden"
-- (Loogen, Ortega-Mallén, Peña, JFP 15(3):431-475, 2005):
-- http://dalila.sip.ucm.es/~ricardo/jfp05b.pdf
--
--   function Buchberger (F = {f1,...,fr}) return G
--       G := F;  P := {(fi,fj) | fi,fj in F, i /= j}
--       while P /= {} do
--           (f,g) <- chooseAPair(P);  P := P - {(f,g)}
--                        G
--           S(f,g) --------> * h  such that h is reduced w.r.t. G
--           if h /= 0 then
--               P := P u {(u,h) | u in G}
--               G := G u {h}
--           end if
--       end while
--       return G
--   end function
--
-- The paper's parallel version farms out the reduction-to-normal-form
--                G
-- step S(f,g) ------>* h to a stateful replicated-worker skeleton
-- (strw) of Eden processes, each backed by an external Maple process
-- (Figure 26). Here that step is just an ordinary sequential function
-- call, `reduce`, exactly as in Figure 25's sequential algorithm --
-- there is no farming out of the reductions.

------------------------------------------------------------------
-- Exact rational coefficients.
--
-- Buchberger's algorithm needs a field to divide leading
-- coefficients; Double would risk an S-polynomial that should
-- cancel to exactly zero coming out as a tiny non-zero float
-- instead, which would break both the "h /= 0" test and the
-- algorithm's termination.
------------------------------------------------------------------

public export
record Rational where
  constructor MkRational
  num : Integer
  den : Integer -- invariant: den > 0, gcd(|num|,den) = 1

gcdI : Integer -> Integer -> Integer
gcdI a b = go (abs a) (abs b)
  where
    go : Integer -> Integer -> Integer
    go x 0 = x
    go x y = go y (x `mod` y)

export
mkRational : Integer -> Integer -> Rational
mkRational n 0 = MkRational 0 1 -- division by zero shouldn't arise; treat as 0
mkRational n d =
  let s = if d < 0 then -1 else 1
      n' = n * s
      d' = d * s
      g  = gcdI n' d'
  in if g == 0 then MkRational 0 1 else MkRational (n' `div` g) (d' `div` g)

export
constR : Integer -> Rational
constR n = MkRational n 1

export
zeroR : Rational
zeroR = constR 0

export
isZeroR : Rational -> Bool
isZeroR r = r.num == 0

export
negR : Rational -> Rational
negR r = MkRational (-(r.num)) r.den

export
addR : Rational -> Rational -> Rational
addR r1 r2 = mkRational (r1.num * r2.den + r2.num * r1.den) (r1.den * r2.den)

export
subR : Rational -> Rational -> Rational
subR r1 r2 = addR r1 (negR r2)

export
mulR : Rational -> Rational -> Rational
mulR r1 r2 = mkRational (r1.num * r2.num) (r1.den * r2.den)

export
recipR : Rational -> Rational
recipR r = mkRational r.den r.num

export
divR : Rational -> Rational -> Rational
divR r1 r2 = mulR r1 (recipR r2)

export
Eq Rational where
  r1 == r2 = r1.num == r2.num && r1.den == r2.den

export
Show Rational where
  show r = if r.den == 1 then show r.num else show r.num ++ "/" ++ show r.den

------------------------------------------------------------------
-- Monomials and polynomials.
--
-- A Monomial is an exponent vector (x1^e1 * x2^e2 * ... * xn^en); a
-- Polynomial is a list of (coefficient, monomial) Terms, kept sorted
-- in *strictly decreasing* order under a fixed monomial order (here:
-- lexicographic, comparing exponents left to right), with distinct
-- monomials and no zero coefficients. Every function below both
-- expects and preserves that invariant.
------------------------------------------------------------------

public export
Monomial : Type
Monomial = List Nat

public export
Term : Type
Term = (Rational, Monomial)

public export
Polynomial : Type
Polynomial = List Term

-- x_i in n variables (0-indexed)
export
var : Nat -> Nat -> Monomial
var i n = map (\j => if i == j then 1 else 0) [0 .. minus n 1]

export
constMonomial : Nat -> Monomial
constMonomial n = replicate n 0

-- lexicographic order on exponent vectors
export
cmpMonomial : Monomial -> Monomial -> Ordering
cmpMonomial []        []        = EQ
cmpMonomial (a :: as) (b :: bs) = case compare a b of
  EQ => cmpMonomial as bs
  o  => o
cmpMonomial _ _ = EQ -- mismatched lengths shouldn't occur in well-formed use

export
mulMonomial : Monomial -> Monomial -> Monomial
mulMonomial = zipWith (+)

export
lcmMonomial : Monomial -> Monomial -> Monomial
lcmMonomial = zipWith max

-- Just (m1/m2) if m2 divides m1 componentwise, else Nothing
export
divMonomial : Monomial -> Monomial -> Maybe Monomial
divMonomial []        []        = Just []
divMonomial (a :: as) (b :: bs) =
  if a >= b then map (minus a b ::) (divMonomial as bs) else Nothing
divMonomial _ _ = Nothing

export
leadingTerm : Polynomial -> Maybe Term
leadingTerm []      = Nothing
leadingTerm (t :: _) = Just t

-- merge two sorted term lists, combining equal monomials and
-- dropping any that cancel to zero
export
addPoly : Polynomial -> Polynomial -> Polynomial
addPoly []       q        = q
addPoly p        []       = p
addPoly ((c1,m1) :: ps) ((c2,m2) :: qs) =
  case cmpMonomial m1 m2 of
    GT => (c1,m1) :: addPoly ps ((c2,m2) :: qs)
    LT => (c2,m2) :: addPoly ((c1,m1) :: ps) qs
    EQ => let c = addR c1 c2
          in if isZeroR c then addPoly ps qs else (c,m1) :: addPoly ps qs

export
negPoly : Polynomial -> Polynomial
negPoly = map (\(c,m) => (negR c, m))

export
subPoly : Polynomial -> Polynomial -> Polynomial
subPoly p q = addPoly p (negPoly q)

-- multiply every term by (coeff * monomial); this preserves the
-- sortedness invariant because lexicographic order is compatible
-- with multiplication (m1 < m2 implies m*m1 < m*m2)
export
scalePoly : Rational -> Monomial -> Polynomial -> Polynomial
scalePoly c m = map (\(c',m') => (mulR c c', mulMonomial m m'))

-- build a polynomial (with integer coefficients) from a list of
-- (coefficient, monomial) pairs, in any order, combining like terms
export
mkPoly : List (Integer, Monomial) -> Polynomial
mkPoly = foldl (\p, (n,m) => addPoly p [(constR n, m)]) []

------------------------------------------------------------------
-- Buchberger's algorithm (Figure 25).
------------------------------------------------------------------

-- S(f,g): cancel the leading terms of f and g against their lcm
export
spoly : Polynomial -> Polynomial -> Polynomial
spoly f g = case (leadingTerm f, leadingTerm g) of
  (Just (cf,mf), Just (cg,mg)) =>
    let mLcm = lcmMonomial mf mg
        qf   = fromMaybe (constMonomial (length mf)) (divMonomial mLcm mf)
        qg   = fromMaybe (constMonomial (length mg)) (divMonomial mLcm mg)
    in subPoly (scalePoly (recipR cf) qf f) (scalePoly (recipR cg) qg g)
  _ => []

-- Reduction of h to normal form w.r.t. G: the standard multivariate
--                                            G
-- division algorithm (Cox-Little-O'Shea), i.e. f -------> * h from
-- Figure 25. Not visibly structurally decreasing to Idris (each
-- step's termination relies on the underlying monomial order being
-- a well-order, which is what actually guarantees Buchberger's
-- algorithm terminates), hence `partial`.
export partial
reduce : Polynomial -> List Polynomial -> Polynomial
reduce p gs = go p []
  where
    findDivisor : Monomial -> List Polynomial -> Maybe (Rational, Monomial, Polynomial)
    findDivisor m []        = Nothing
    findDivisor m (g :: rest) = case leadingTerm g of
      Nothing        => findDivisor m rest
      Just (cg, mg) => case divMonomial m mg of
        Just q  => Just (cg, q, g)
        Nothing => findDivisor m rest

    go : Polynomial -> Polynomial -> Polynomial
    go []             acc = reverse acc
    go ((c,m) :: ts)  acc = case findDivisor m gs of
      Just (cg, q, g) => go (subPoly ((c,m) :: ts) (scalePoly (divR c cg) q g)) acc
      Nothing         => go ts ((c,m) :: acc)

export
allPairs : List a -> List (a, a)
allPairs []        = []
allPairs (x :: xs) = map (\y => (x,y)) xs ++ allPairs xs

-- Buchberger's algorithm itself, matching Figure 25's pseudocode
-- clause for clause: `loop` is the `while P /= {} do ... end while`
-- loop, with `basis` playing the role of G and its argument list the
-- role of P.
export partial
buchberger : List Polynomial -> List Polynomial
buchberger fs = loop fs (allPairs fs)
  where
    loop : List Polynomial -> List (Polynomial, Polynomial) -> List Polynomial
    loop basis []               = basis
    loop basis ((f,g) :: pairs) =
      let h = reduce (spoly f g) basis
      in if null h
           then loop basis pairs
           else loop (h :: basis) (pairs ++ map (\u => (u,h)) basis)

------------------------------------------------------------------
-- Minimal / reduced Gröbner bases.
--
-- `buchberger` is a direct port of Figure 25, which just returns G
-- as accumulated by the loop -- a valid Gröbner basis, but not
-- necessarily the minimal or (canonical) *reduced* one, since it can
-- contain elements whose leading term is divisible by another
-- element's leading term (hence redundant). This section adds the
-- standard textbook cleanup on top, which the paper doesn't cover.
------------------------------------------------------------------

-- every way to pick one element out of a list, paired with the rest
selects : List a -> List (a, List a)
selects []        = []
selects (x :: xs) = (x, xs) :: map (\(y,ys) => (y, x :: ys)) (selects xs)

-- does h's leading term divide m?
dividesLT : Polynomial -> Monomial -> Bool
dividesLT h m = case leadingTerm h of
  Nothing      => False
  Just (_, mh) => isJust (divMonomial m mh)

-- drop any basis element whose leading term is divisible by another
-- element's leading term: what remains is still a Gröbner basis for
-- the same ideal (a standard consequence of Buchberger's criterion).
export
minimizeBasis : List Polynomial -> List Polynomial
minimizeBasis basis = map fst (filter keep (selects basis))
  where
    keep : (Polynomial, List Polynomial) -> Bool
    keep (g, rest) = case leadingTerm g of
      Nothing      => False
      Just (_, mg) => not (any (\h => dividesLT h mg) rest)

-- reduce every basis element's *non-leading* terms against the rest
-- of the (already minimal) basis; the leading term itself is left
-- alone, since minimality already guarantees no other element's
-- leading term divides it.
export partial
fullyReduce : List Polynomial -> List Polynomial
fullyReduce basis = map reduceOne (selects basis)
  where
    reduceOne : (Polynomial, List Polynomial) -> Polynomial
    reduceOne ([], _)              = []
    reduceOne (lt :: tailTerms, rest) = lt :: reduce tailTerms rest

-- scale a polynomial so its leading coefficient is 1
export
monic : Polynomial -> Polynomial
monic []              = []
monic p@((c,m) :: _) = scalePoly (recipR c) (constMonomial (length m)) p

-- minimal and fully reduced, but *not* normalised to monic -- e.g.
-- for the circle-and-line example this gives {x-y, 2y^2-1} rather
-- than {x-y, y^2-1/2}. Every basis element is unique only up to a
-- scalar multiple without the monic step, so this isn't quite the
-- textbook-canonical reduced Gröbner basis, but it's still minimal
-- and fully reduced, and it avoids introducing fractions that
-- weren't already in the input.
export partial
reducedBasisNonMonic : List Polynomial -> List Polynomial
reducedBasisNonMonic basis = fullyReduce (minimizeBasis basis)

-- the canonical reduced Gröbner basis: minimal, fully reduced, monic
export partial
reducedBasis : List Polynomial -> List Polynomial
reducedBasis basis = map monic (reducedBasisNonMonic basis)

------------------------------------------------------------------
-- Pretty-printing, for readable test output.
------------------------------------------------------------------

showVarExp : String -> Nat -> String
showVarExp v 0 = ""
showVarExp v 1 = v
showVarExp v e = v ++ "^" ++ show e

showMonomial : List String -> Monomial -> String
showMonomial vars m =
  case filter (/= "") (zipWith showVarExp vars m) of
    []    => ""
    parts => concat (intersperse "*" parts)

absR : Rational -> Rational
absR r = if r.num < 0 then negR r else r

isNegR : Rational -> Bool
isNegR r = r.num < 0

-- unsigned magnitude of a term, e.g. (-3,[2,0]) -> "3*x^2"
showTermMag : List String -> Term -> String
showTermMag vars (c,m) = case showMonomial vars m of
  ""  => show (absR c)
  mon => if absR c == constR 1 then mon else show (absR c) ++ "*" ++ mon

export
showPoly : List String -> Polynomial -> String
showPoly vars []         = "0"
showPoly vars (t :: ts) =
  let first = (if isNegR (fst t) then "-" else "") ++ showTermMag vars t
      rest  = concat (map (\u => (if isNegR (fst u) then " - " else " + ") ++ showTermMag vars u) ts)
  in first ++ rest

f1 : Polynomial
f1 = mkPoly [(1, [2,0]), (1, [0,2]), (-1, [0,0])]   -- x^2 + y^2 - 1

f2 : Polynomial
f2 = mkPoly [(1, [1,0]), (-1, [0,1])]               -- x - y

-- :exec traverse_ (putStrLn . showPoly ["x","y"]) (reducedBasisNonMonic (buchberger [mkPoly [(1,[2,0]),(1,[0,2]),(-1,[0,0])], mkPoly [(1,[1,0]),(-1,[0,1])]]))
-- 2*y^2 - 1
-- x - y

-- Benchmark: a fully-coupled dense quadratic system in n variables
-- (n equations, generic-looking coefficients, no linear-elimination
-- shortcut like the f1/f2 example above), used to give this naive
-- Buchberger implementation -- no chain/product-criterion pair
-- elimination -- real work to do. Equation i is
--   sum_j (i+j+1)*x_j^2 + sum_j (j+1)*x_j - (i+1) = 0
-- n=15 takes roughly 30 seconds on a typical laptop.
export
denseEq : Nat -> Nat -> Polynomial
denseEq n i =
  mkPoly (  map (\j => (cast (i+j+1), mulMonomial (var j n) (var j n))) [0 .. minus n 1]
         ++ map (\j => (cast (j+1), var j n)) [0 .. minus n 1]
         ++ [(-(cast (i+1)), constMonomial n)] )

export
denseSystem : Nat -> List Polynomial
denseSystem n = map (denseEq n) [0 .. minus n 1]

partial
main : IO ()
main = printLn (length (buchberger (denseSystem 15)))