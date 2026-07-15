module Mandelbrot

import Complex
import System.File
import Data.String

-- Ported from the sequential core of nofib's parallel/mandel benchmark
-- (https://github.com/ghc/nofib/blob/master/parallel/mandel/Mandel.lhs),
-- dropping the Strategies-based parallelism (parBuffer / parListTreeLike)
-- and the PortablePixmap rendering, which are out of scope here.

%default total

-- Same viewport/resolution/iteration-limit as nofib's own benchmark
-- invocation (SRC_RUNTEST_OPTS in parallel/mandel/Makefile):
--   -2.0 -2.0 2.0 2.0 1024 1024 256
-- the classic full view of the Mandelbrot set, on a 1024x1024 grid,
-- 256 iterations. This also makes the viewport-derived `radius`
-- below come out to the conventional escape radius of 2.0.
x0, y0, x1, y1 : Double
x0 = -2.0
y0 = -2.0
x1 = 2.0
y1 = 2.0

width, height : Int
width = 100
height = 100

maxIters : Int
maxIters = 256

-- mandel c = c : map (\z -> z*z + c) infiniteMandel
-- one step of the orbit of c: z |-> z*z + c
export
step : Complex Double -> Complex Double -> Complex Double
step c z = z * z + c

export
diverge : Complex Double -> Double -> Bool
diverge z radius = magnitude z > radius

-- whenDiverge limit radius c = walkIt (take limit (mandel c))
--   where walkIt []     = 0                   -- Converged
--         walkIt (x:xs) | diverge x radius = 0 -- Diverged
--                       | otherwise        = 1 + walkIt xs
-- Walks up to `limit` elements of the orbit c, c*c+c, (c*c+c)*(c*c+c)+c, ...
-- returning the index of the first element that diverges, or `limit`
-- if none do within that many steps (i.e. c is taken to be in the set).
export
whenDiverge : (limit : Nat) -> (radius : Double) -> (c : Complex Double) -> Nat
whenDiverge limit radius c = walkIt limit c
  where
    walkIt : Nat -> Complex Double -> Nat
    walkIt Z     x = 0
    walkIt (S f) x =
      if diverge x radius
        then 0
        else 1 + walkIt f (step c x)

-- windowToViewport s t
--      = ((x + (((coerce s) * (x' - x)) / (fromInteger screenX))) :+
--         (y + (((coerce t) * (y' - y)) / (fromInteger screenY))))
export
windowToViewport : Int -> Int -> Complex Double
windowToViewport s t =
  (x0 + (cast s * (x1 - x0) / cast width)) :+ (y0 + (cast t * (y1 - y0) / cast height))

-- radius = (max (x'-x) (y'-y)) / 2.0
radius : Double
radius = max (x1 - x0) (y1 - y0) / 2.0

-- result = [ whenDiverge lIMIT radius (windowToViewport s t) | s<-[1..screenX] ]
--          | t <- [1..screenY] ]
-- (the sequential shape of nofib's `result`, before parListTreeLike/parBuffer)
export
mandelRow : Int -> List Nat
mandelRow t = map (\s => whenDiverge (cast maxIters) radius (windowToViewport s t)) [1..width]

export
mandelGrid : List (List Nat)
mandelGrid = map mandelRow [1..height]

-- Checksum of the escape counts across the whole image, used to validate a parallel implementation against this
-- sequential one.
export
run : Nat
run = foldl (\acc, row => acc + sum row) 0 mandelGrid

-- prettyRGB s = let t = (lIMIT - s) in (s,t,t)
-- nofib's own colour mapping: red channel is the escape count, green
-- and blue both fill in the remainder, so points in the set (s ==
-- maxIters) come out black and fast-escaping points come out red.
export
prettyRGB : Nat -> (Int, Int, Int)
prettyRGB s =
  let r = cast s
      t = maxIters - r
  in (r, t, t)

-- A P3 (plain ASCII) PPM: same pixel data as nofib's P6 pixmap, just
-- text instead of raw bytes, so it needs no binary file-IO machinery
-- and has no risk of a 256 escape count overflowing a single byte.
export
ppm : String
ppm =
  "P3\n" ++
  "# Portable pixmap created by Mandelbrot.idr\n" ++
  show width ++ " " ++ show height ++ "\n" ++
  show maxIters ++ "\n" ++
  concatMap ppmRow mandelGrid
  where
    ppmPixel : Nat -> String
    ppmPixel s = let (r, g, b) = prettyRGB s in show r ++ " " ++ show g ++ " " ++ show b ++ "  "

    ppmRow : List Nat -> String
    ppmRow row = concatMap ppmPixel row ++ "\n"

export
writePPM : String -> IO ()
writePPM path = do
  Right () <- writeFile path ppm
    | Left err => putStrLn ("Error writing " ++ path ++ ": " ++ show err)
  putStrLn ("Wrote " ++ path)

-- Cheap terminal preview: one character per pixel, bucketed by
-- escape count on a log-ish scale; points still in the set (escape
-- count == maxIters) render as blank space.
export
shade : Nat -> Char
shade n =
  if n >= cast maxIters then ' '
  else if n < 2   then '.'
  else if n < 5   then ':'
  else if n < 10  then '-'
  else if n < 20  then '='
  else if n < 40  then '+'
  else if n < 80  then '*'
  else if n < 160 then '#'
  else '@'

export
asciiArt : String
asciiArt = unlines (map (\row => pack (map shade row)) mandelGrid)

export
printMandelbrot : IO ()
printMandelbrot = putStr asciiArt
