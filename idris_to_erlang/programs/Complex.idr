module Complex


infix 6 :+

-- Define the Complex type with an infix constructor for the imaginary part
public export
data Complex a = (:+) a a

-- Implementation of Show to format complex numbers
export
Show a => Show (Complex a) where
  show (x :+ y) = show x ++ " + " ++ show y ++ "i"

mutual
 export
 (Num a, Neg a) => Neg (Complex a) where
    negate (r :+ i) = (-r) :+ (-i)

    (r1 :+ i1) - (r2 :+ i2) = (r1 - r2) :+ (i1 - i2)

 export
 (Neg a, Num a) => Num (Complex a) where
    (+) (a :+ b) (c :+ d) = (a + c) :+ (b + d)
    (*) (a :+ b) (c :+ d) = (a * c - b * d) :+ (a * d + b * c)

    fromInteger x = fromInteger x :+ 0

export
(Num a, FromDouble a) => FromDouble (Complex a) where
    fromDouble x = fromDouble x :+ 0

export
magnitude : Complex Double -> Double
magnitude (x :+ y) = Prelude.Types.sqrt (x * x + y * y)

export
atan2 : Double -> Double -> Double
atan2 y x =
  if x > 0 then atan (y / x)
  else if x < 0 && y >= 0 then atan (y / x) + pi
  else if x < 0 then atan (y / x) - pi
  else if x == 0 && y > 0 then pi / 2
  else if x == 0 && y < 0 then -(pi / 2)
  else 0.0

export
phase : Complex Double -> Double
phase (0.0 :+ 0.0) = 0.0
phase (x :+ y) = atan2 y x

export
polar : Complex Double -> (Double, Double)
polar z = (magnitude z, phase z)

export
abs : Complex Double -> Complex Double
abs z = magnitude z :+ 0.0

export
sqrt : Complex Double -> Complex Double
sqrt (x :+ y) =
  let m  = magnitude (x :+ y)
      re = Prelude.Types.sqrt ((m + x) / 2)
      im = Prelude.Types.sqrt ((m - x) / 2)
  in re :+ (if y < 0 then -im else im)

-- Example usage
export
c1 : Complex Double
c1 = 3.5 :+ 2.0

export
c2 : Complex Double
c2 = 1.2 :+ 4.5

-- Sum would be: 4.7 + 6.5i

export
realPart : Complex a -> a
realPart (x :+ _) = x

export
imagPart : Complex a -> a
imagPart (_ :+ y) = y

export
conjugate : (Num a, Neg a) => Complex a -> Complex a
conjugate (x :+ y) = x :+ (-y)

export
mkPolar : Double -> Double -> Complex Double
mkPolar r theta = (r * cos theta) :+ (r * sin theta)

export
cis : Double -> Complex Double
cis theta = cos theta :+ sin theta
