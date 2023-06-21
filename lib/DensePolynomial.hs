module DensePolynomial where

data DensePolynomial a = DensePolynomial [a]
  deriving (Show, Eq)

pad :: Num a => DensePolynomial a -> Int -> DensePolynomial a
pad (DensePolynomial xs) n = DensePolynomial $ replicate n 0 ++ xs

trim :: Num a => Eq a => DensePolynomial a -> DensePolynomial a
trim poly@(DensePolynomial (h:t))
  | h == 0 = trim $ DensePolynomial t
  | otherwise = poly

trim (DensePolynomial []) = (DensePolynomial [])

add :: Num a => Eq a => DensePolynomial a -> DensePolynomial a -> DensePolynomial a
add a@(DensePolynomial aCoefs) b@(DensePolynomial bCoefs)
  | lenA > lenB = add a $ pad b (lenA - lenB)
  | lenB > lenA = add (pad a $ lenB - lenA) $ b
  | otherwise = DensePolynomial (zipWith (+) aCoefs bCoefs)
    where lenA = length aCoefs
          lenB = length bCoefs


sub :: Num a => Eq a => DensePolynomial a -> DensePolynomial a -> DensePolynomial a
sub a@(DensePolynomial aCoefs) b@(DensePolynomial bCoefs)
  | lenA > lenB = sub a $ pad b (lenA - lenB)
  | lenB > lenA = sub (pad a $ lenB - lenA) $ b
  | otherwise = trim (DensePolynomial (zipWith (-) aCoefs bCoefs))
    where lenA = length aCoefs
          lenB = length bCoefs

scale :: Num a => Eq a => DensePolynomial a -> a -> DensePolynomial a
scale (DensePolynomial polyCoefs) n = DensePolynomial scaledPolyCoefs
  where scaledPolyCoefs = map (* n) polyCoefs

mul :: Num a => Eq a => DensePolynomial a -> DensePolynomial a -> DensePolynomial a 
mul a@(DensePolynomial aCoefs) b@(DensePolynomial bCoefs) = undefined





