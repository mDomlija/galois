{-# LANGUAGE InstanceSigs #-}
module DensePolynomial where

data DensePolynomial a = DensePolynomial [a]
  deriving (Show, Eq)


instance (Num a, Eq a) => Num (DensePolynomial a) where
  (+) :: (Num a, Eq a) => DensePolynomial a -> DensePolynomial a -> DensePolynomial a
  (+) = add
  (*) :: (Num a, Eq a) => DensePolynomial a -> DensePolynomial a -> DensePolynomial a
  (*) = mul
  abs :: (Num a, Eq a) => DensePolynomial a -> DensePolynomial a
  abs (DensePolynomial coefs) = DensePolynomial (map Prelude.abs coefs)
  signum :: (Num a, Eq a) => DensePolynomial a -> DensePolynomial a
  signum (DensePolynomial (h:t)) = DensePolynomial [Prelude.signum h]
  signum (DensePolynomial []) = DensePolynomial []
  fromInteger :: (Num a, Eq a) => Integer -> DensePolynomial a
  fromInteger n = DensePolynomial [Prelude.fromInteger n]
  (-) :: (Num a, Eq a) => DensePolynomial a -> DensePolynomial a -> DensePolynomial a
  (-) = sub

--data Polynomial a = Dense (DensePolynomial a) 

pad :: Num a => DensePolynomial a -> Int -> DensePolynomial a
pad (DensePolynomial xs) n = DensePolynomial $ replicate n 0 ++ xs

trim :: Num a => Eq a => DensePolynomial a -> DensePolynomial a
trim poly@(DensePolynomial (h:t))
  | h == 0 = trim $ DensePolynomial t
  | otherwise = poly

trim (DensePolynomial []) = DensePolynomial []

add :: Num a => Eq a => DensePolynomial a -> DensePolynomial a -> DensePolynomial a
add a@(DensePolynomial aCoefs) b@(DensePolynomial bCoefs)
  | lenA > lenB = add a $ pad b (lenA - lenB)
  | lenB > lenA = add (pad a $ lenB - lenA) b
  | otherwise = DensePolynomial (zipWith (+) aCoefs bCoefs)
    where lenA = length aCoefs
          lenB = length bCoefs


sub :: Num a => Eq a => DensePolynomial a -> DensePolynomial a -> DensePolynomial a
sub a@(DensePolynomial aCoefs) b@(DensePolynomial bCoefs)
  | lenA > lenB = sub a $ pad b (lenA - lenB)
  | lenB > lenA = sub (pad a $ lenB - lenA) b
  | otherwise = trim (DensePolynomial (zipWith (-) aCoefs bCoefs))
    where lenA = length aCoefs
          lenB = length bCoefs

scale :: Num a => Eq a => DensePolynomial a -> a -> DensePolynomial a
scale (DensePolynomial polyCoefs) n = DensePolynomial scaledPolyCoefs
  where scaledPolyCoefs = map (* n) polyCoefs




mul :: Num a => Eq a => DensePolynomial a -> DensePolynomial a -> DensePolynomial a
mul a@(DensePolynomial aCoefs) b@(DensePolynomial bCoefs) = _mul a b' zeros 0
  where lenA = length aCoefs
        lenB = length bCoefs
        b' = DensePolynomial (reverse bCoefs)
        zeros = replicate lenA 0

_mul :: Num a => Eq a => DensePolynomial a -> DensePolynomial a -> [a] -> Int -> DensePolynomial a
--b has to be reveresed beforehand!!!!!!!!!!!!!!!!!

_mul _ (DensePolynomial []) solution _ = trim $ DensePolynomial solution
_mul a@(DensePolynomial aCoefs) b@(DensePolynomial (coef : bTail)) solution offset = _mul a newB newSolution $ offset + 1
  where toAdd = reverse $ replicate offset 0 ++ map (* coef) (reverse aCoefs)
        newSolution = 0 : zipWith (+) solution toAdd
        newB = DensePolynomial bTail


divmod :: (Fractional a, RealFrac a) => Eq a => DensePolynomial a -> DensePolynomial a -> (DensePolynomial a, DensePolynomial a)
divmod a@(DensePolynomial aCoefs) b@(DensePolynomial bCoefs) = _divmod a b q
  where q = replicate lenA 0
        lenA = length aCoefs

_divmod :: (Fractional a, RealFrac a) => Eq a => DensePolynomial a -> DensePolynomial a -> [a] -> (DensePolynomial a, DensePolynomial a)
_divmod a@(DensePolynomial aCoefs) b@(DensePolynomial bCoefs) q
  | lenA >= lenB = _divmod aNew b qNew
  | otherwise = (trim (DensePolynomial (reverse q)), a)
    where lenA = length aCoefs
          lenB = length bCoefs
          coef = head aCoefs / head bCoefs
          bHelp = map (* coef) bCoefs ++ replicate (lenA - lenB) 0
          aNew =  trim $ DensePolynomial (map (round' 6) $ zipWith (-) aCoefs bHelp)
          qNew = replaceAtIndex (lenA - lenB) coef q 


replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n newVal xs = before ++ (newVal : after)
  where (before, _:after) = splitAt n xs

round' :: (Fractional a, RealFrac a) =>  Integer -> a -> a
round' sg num = (fromIntegral . round $ num * f) / f
    where f = 10^sg







