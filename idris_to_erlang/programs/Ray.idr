module Ray

import System
import Data.String
import Data.Maybe

-- Direct sequential port of nofib's parallel/ray/Main.lhs -- the ray tracer
-- from Paul Kelly's book, adapted by Greg Michaelson for SML,
-- converted to (parallel) Haskell by Kevin Hammond
-- (https://github.com/ghc/nofib/blob/master/parallel/ray/Main.lhs).
-- The only real change from a direct transliteration is dropping the
-- Control.Parallel.Strategies parBuffer wrapper in findImpacts, which
-- was the sole source of parallelism: it's a plain sequential `map`
-- here.

public export
Coord : Type
Coord = (Double, Double, Double)

public export
Vector : Type
Vector = (Double, Double, Double)

public export
Ray : Type
Ray = (Coord, Coord)

-- Objects: just polygons at present. Polygon(i,N,Vs) is a polygon
-- with identifier i, with normal vector N and vertices Vs.
public export
data Object = Polygon Int Vector (List Coord)

public export
data Impact = MkImpact Double Int | NoImpact

-- small value used to prevent real rounding errors
jot : Double
jot = 1.0e-8

-- in_poly_range (p,q,r) Vs, where Vs is a list of the vertices of a
-- Polygon, tests whether the point (p,q,r) lies within the 'range'
-- of the polygon: p must be less than the largest x component
-- within Vs and greater than the smallest, similarly for q and r.
-- Returns (xbig,xsmall,ybig,ysmall,zbig,zsmall) where xbig is true
-- if p is greater than all polygon x components, xsmall is true if
-- p is smaller than all polygon x components, etc.
in_poly_range : Coord -> List Vector -> (Bool,Bool,Bool,Bool,Bool,Bool)
in_poly_range (p,q,r) [] = (True,True,True,True,True,True)
in_poly_range (p,q,r) ((u,v,w) :: vs) =
  let (xbig,xsmall,ybig,ysmall,zbig,zsmall) = in_poly_range (p,q,r) vs
  in (xbig && p>u+jot, xsmall && p<u-jot,
      ybig && q>v+jot, ysmall && q<v-jot,
      zbig && r>w+jot, zsmall && r<w-jot)

-- cross_dot_sign (a,b,c) (d,e,f) (A,B,C) returns -1 or 1 according
-- to the sign of the dot product of (P,Q,R) & (A,B,C), where (P,Q,R)
-- is the cross product of (a,b,c) & (d,e,f) and (A,B,C) is the
-- normal to a polygon, so this returns 1 if the cross product
-- points 'up' from the polygon and -1 if it points 'down'.
cross_dot_sign : Vector -> Vector -> Vector -> Int
cross_dot_sign (a,b,c) (d,e,f) (a',b',c') =
  let p  = b*f - e*c
      q  = d*c - a*f
      r  = a*e - d*b
      cd = p*a' + q*b' + r*c'
  in if cd < 0 then -1 else 1

-- really_in_poly (p,q,r) (A,B,C) Vs tests if point (p,q,r) is inside
-- the polygon with normal (A,B,C) and vertices Vs: test that
-- cross_dot_sign returns the same sign for all edges. As in the
-- original, this is undefined for a vertex list of length 0 or 1
-- (a "polygon" always has at least 2 vertices in practice).
partial
really_in_poly : Vector -> Vector -> List Coord -> (Bool, Int)
really_in_poly (p,q,r) (a,b,c) [(x1,y1,z1),(x2,y2,z2)] =
  (True, cross_dot_sign (x2-p,y2-q,z2-r) (x2-x1,y2-y1,z2-z1) (a,b,c))
really_in_poly (p,q,r) (a,b,c) ((x1,y1,z1) :: (x2,y2,z2) :: vs) =
  let (in_poly, s) = really_in_poly (p,q,r) (a,b,c) ((x2,y2,z2) :: vs)
      s1            = cross_dot_sign (x2-p,y2-q,z2-r) (x2-x1,y2-y1,z2-z1) (a,b,c)
  in if in_poly
       then if s1 == s then (True, s1) else (False, 0)
       else (in_poly, s)

-- in_poly_test (p,q,r) (A,B,C) Vs tests if point (p,q,r) is inside
-- the polygon with vertices Vs & normal vector (A,B,C): first test
-- if p,q,r are inside 'range' of the polygon vertices, and only if
-- that passes, do the accurate test.
partial
in_poly_test : Coord -> Vector -> List Coord -> Bool
in_poly_test (p,q,r) (a,b,c) vs =
  let (b1,b2,b3,b4,b5,b6) = in_poly_range (p,q,r) vs
      (in_poly, _)        = really_in_poly (p,q,r) (a,b,c) vs
  in if b1 || b2 || b3 || b4 || b5 || b6 then False else in_poly

-- Following functions are after Kelly's ray-tracing example in
-- 'Functional Programming for Loosely Coupled Multiprocessors'.

-- Return impact for ray and polygon: calculate point (p,q,r) where
-- the ray intersects the plane of the polygon, then test if that
-- point lies inside or outside the polygon. If inside, return
-- Impact(distance,i) giving distance along the ray and polygon id,
-- otherwise return NoImpact.
partial
testForImpact : Ray -> Object -> Impact
testForImpact ((u,v,w),(l,m,n)) (Polygon i (a,b,c) vs@((px,py,pz) :: _)) =
  let distance = (a*(px-u)+b*(py-v)+c*(pz-w)) / (a*l+b*m+c*n)
      p = u + distance*l
      q = v + distance*m
      r = w + distance*n
  in if in_poly_test (p,q,r) (a,b,c) vs then MkImpact distance i else NoImpact

-- return impact with smaller distance
earlier : Impact -> Impact -> Impact
earlier NoImpact NoImpact = NoImpact
earlier i1       NoImpact = i1
earlier NoImpact i2       = i2
earlier i1@(MkImpact d1 _) i2@(MkImpact d2 _) = if d1 <= d2 then i1 else i2

insert : (Impact -> Impact -> Impact) -> Impact -> List Impact -> Impact
insert f d []        = d
insert f d (x :: xs) = f x (insert f d xs)

partial
firstImpact : List Object -> Ray -> Impact
firstImpact os r = earliest (map (testForImpact r) os)
  where
    earliest : List Impact -> Impact
    earliest = insert earlier NoImpact

-- Sequential: nofib's own version wraps this map in `parBuffer`.
export partial
findImpacts : List Ray -> List Object -> List Impact
findImpacts rays objects = map (firstImpact objects) rays

-- Functions to generate a list of rays.
--
-- generateRays Detail X Y Z generates a list of Detail*Detail rays
-- emanating from the point X, Y, Z. The rays are formed by
-- projecting from the view point (X,Y,Z) through a grid of points
-- (of side Detail) held at a distance of 4 units from the viewpoint.
-- The grid is positioned so that rays in the centre of the view are
-- directed toward the origin.

root : Double -> Double -> Double
root x r =
  if x == 0 then 0
  else if abs ((r*r-x)/x) < 0.0000001 then r
  else root x ((r+x/r)/2.0)

vadd : Coord -> Vector -> Vector
vadd (a,b,c) (d,e,f) = (a+d,b+e,c+f)

vmult : Double -> Vector -> Vector
vmult n (u,v,w) = (n*u,n*v,n*w)

ray_points : (Int,Int) -> Int -> Coord -> Vector -> Vector -> List Vector
ray_points (i,j) detail (p,q,r) vx vy =
  if j == detail then []
  else if i == detail then ray_points (0,j+1) detail (p,q,r) vx vy
  else
    let ivx = vmult (cast i / cast (detail-1)) vx
        jvy = vmult (cast j / cast (detail-1)) vy
        ps  = vadd (vadd (p,q,r) ivx) jvy
    in ps :: ray_points (i+1,j) detail (p,q,r) vx vy

export
generateRays : Int -> Double -> Double -> Double -> List Ray
generateRays det x y z =
  let d                                  = root (x*x+y*y+z*z) 1.0
      (vza,vzb,vzc)                      = ((-4.0)*x/d,(-4.0)*y/d,(-4.0)*z/d)
      ab                                 = root (vza*vza + vzb*vzb) 1.0
      (vxa,vxb,vxc)                      = (vzb/ab,(-vza)/ab,0)
      (ya,yb,yc)                         = (vzb*vxc-vxb*vzc,vxa*vzc-vza*vxc,vza*vxb-vxa*vzb)
      ysize                              = root (ya*ya+yb*yb+yc*yc) 1.0
      (vya,vyb,vyc)                      = (ya/ysize,yb/ysize,yc/ysize)
      ((vxa',vxb',vxc'),(vya',vyb',vyc')) =
        if vyc > 0
          then ((-vxa,-vxb,-vxc),(-vya,-vyb,-vyc))
          else ((vxa,vxb,vxc),(vya,vyb,vyc))
      (p,q,r)                            = (x+vza-(vxa'+vya')/2.0,
                                             y+vzb-(vxb'+vyb')/2.0,
                                             z+vzc-(vxc'+vyc')/2.0)
      rps = ray_points (0,0) det (p,q,r) (vxa',vxb',vxc') (vya',vyb',vyc')
  in map (\(x',y',z') => ((x,y,z),(x'-x,y'-y,z'-z))) rps

show_imps : Int -> Int -> List Impact -> String
show_imps dv i []          = ""
show_imps dv 0 imps        = "\n" ++ show_imps dv dv imps
show_imps dv i (imp::imps) = simp imp ++ " " ++ show_imps dv (i-1) imps
  where
    simp : Impact -> String
    simp NoImpact     = "."
    simp (MkImpact _ p) = show p

-- top level function
export partial
top : Int -> Double -> Double -> Double -> List Object -> String
top detail viewx viewy viewz scene =
  show_imps detail detail imps
  where
    rays = generateRays detail viewx viewy viewz
    imps = findImpacts rays scene

-- Example scene: consists of 6 squares, arranged something like:
--
--                z
--                |
--             /\ |
--            /  \| /\
--           |\ 5/|/  \
--           | \/ |\ 2/|
--           |6|4 | \/ |
--           \ | / \3|1/
--            \|/   \|/
--             /     \
--            /       \
--          y          x
export
sc : List Object
sc =
  [ Polygon 1 (1.0,0.0,0.0) [(1.0,0.0,-0.5), (1.0,0.0,0.5),   (1.0,-1.0,0.5), (1.0,-1.0,-0.5), (1.0,0.0,-0.5)]
  , Polygon 2 (0.0,0.0,1.0) [(1.0,0.0,0.5),  (0.0,0.0,0.5),   (0.0,-1.0,0.5), (1.0,-1.0,0.5),  (1.0,0.0,0.5)]
  , Polygon 3 (0.0,1.0,0.0) [(0.0,0.0,-0.5), (0.0,0.0,0.5),   (1.0,0.0,0.5),  (1.0,0.0,-0.5),  (0.0,0.0,-0.5)]
  , Polygon 4 (1.0,0.0,0.0) [(0.0,0.0,-0.5), (0.0,1.3,-0.5),  (0.0,1.3,0.8),  (0.0,0.0,0.8),   (0.0,0.0,-0.5)]
  , Polygon 5 (0.0,0.0,1.0) [(0.0,0.0,0.8),  (0.0,1.3,0.8),   (-1.3,1.3,0.8), (-1.3,0.0,0.8),  (0.0,0.0,0.8)]
  , Polygon 6 (0.0,1.0,0.0) [(0.0,1.3,-0.5), (-1.3,1.3,-0.5), (-1.3,1.3,0.8), (0.0,1.3,0.8),   (0.0,1.3,-0.5)]
  ]

-- nofib's own invocation is just `[detail] <- getArgs; top detail
-- 10.0 7.0 6.0 sc`; Idris2's getArgs includes the executable name as
-- argv[0], so the detail argument is the *second* element here.
partial
main : IO ()
main = do
  args <- getArgs
  let detail = case args of
                 (_ :: d :: _) => fromMaybe 10 (parsePositive d)
                 _             => 10
  putStr (top detail 10.0 7.0 6.0 sc)

-- run with something like: :exec putStr (top 100 10.0 7.0 6.0 sc)
