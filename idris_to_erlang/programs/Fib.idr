module Fib

import Data.List
import Data.Vect
import Pipar2

public export
fib : Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

public export
fibDC : (Int, Int) -> Int
fibDC (0, t) = 0
fibDC (1, t) = 1
fibDC (n, t) =
  case n < t of
    True => fib n
    False => (snd ((<$>) (proc fibDC <#> (n - 1, t)))) + (snd ((<$>) (proc fibDC <#> (n - 2, t))))
  -- if n < t
  --   then fib n
  --   else
  --     let p = proc fibDC in
  --     let r1 = p <#> (n - 1, t) in
  --     let r2 = p <#> (n - 2, t) in
  --     let (s1, n1) = (<$>) r1 in
  --     let (s2, n2) = (<$>) r2 in
      -- (snd ((<$>) (proc fibDC <#> (n - 1, t)))) + (snd ((<$>) (proc fibDC <#> (n - 2, t))))
        -- (<$> ?h1) + (<$> ?h2)
        -- ?h
      -- (<$> proc fibDC <#> (n - 1, t)) + (<$> proc fibDC <#> (n - 2, t))

public export
runFibSeq : Int -> Int
runFibSeq size = fib size

public export
runFibDC : Int -> Int -> Int -> Int
runFibDC nw thres size = fibDC (size, thres)
