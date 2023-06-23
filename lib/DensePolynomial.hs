{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
module DensePolynomial where

import NumberUtils

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

degree :: DensePolynomial a -> Int
degree (DensePolynomial coefs) = length coefs

leadingCoefficient :: Num a => DensePolynomial a -> a
leadingCoefficient (DensePolynomial (lead : tail)) = lead
leadingCoefficient (DensePolynomial []) = 0

pseudoDivisonFactor :: RealFrac a => DensePolynomial a -> DensePolynomial a -> Integer
pseudoDivisonFactor a@(DensePolynomial aCoefs) b@(DensePolynomial bCoefs)
  | degree a >= degree b = round $ beta ^ l
  | otherwise = pseudoDivisonFactor b a
    where beta = leadingCoefficient b
          l = degree a - degree b + 1

pseudoDivmod :: RealFrac a => DensePolynomial a -> DensePolynomial a -> (DensePolynomial a, DensePolynomial a)
pseudoDivmod a b = divmod (a * fromInteger (pseudoDivisonFactor a b)) b

derivative :: (Num a, Eq a) => DensePolynomial a -> DensePolynomial a
derivative a@(DensePolynomial coefs) = _derivative t [] 1
  where (h : t) = reverse coefs
        _derivative (h : t) solution curr = _derivative t ( h * curr : solution) (curr + 1)
        _derivative [] [] _ = DensePolynomial []
        _derivative [] solution _ = DensePolynomial solution

cont :: RealFrac a => DensePolynomial a -> Integer
cont (DensePolynomial []) = 1
cont (DensePolynomial xs) = gcdMultiple xs

pp :: RealFrac a => DensePolynomial a -> DensePolynomial a
pp (DensePolynomial []) = DensePolynomial []
pp (DensePolynomial [h]) = DensePolynomial [1]
pp poly@(DensePolynomial coefs) = res
  where (res, _) = divmod poly $ DensePolynomial [fromInteger cPoly]
        cPoly = cont poly

primitiveEuclidian :: RealFrac a => DensePolynomial a -> DensePolynomial a -> DensePolynomial a
primitiveEuclidian a b = _primitiveEuclidian a b (pp a) (pp b)


_primitiveEuclidian :: RealFrac a => DensePolynomial a -> DensePolynomial a -> DensePolynomial a -> DensePolynomial a -> DensePolynomial a
_primitiveEuclidian a b c (DensePolynomial []) = c * gamma
  where gamma = fromInteger $ gcd' (cont a) (cont b)
_primitiveEuclidian a b c d = _primitiveEuclidian a b newC newD
  where (_, r) = pseudoDivmod c d
        newC = d
        newD = pp r

exp :: (Num a, Eq a) => DensePolynomial a -> Integer -> DensePolynomial a
exp a n = _exp a n a

_exp :: (Num a, Eq a) =>DensePolynomial a -> Integer -> DensePolynomial a -> DensePolynomial a
_exp a n sol
  | n > 1 = _exp a (n-1) (sol * a)
  | n == 1 = sol
  | n == 0 = DensePolynomial [1]


squareFreeFactorization a = squareFreeFactorizationImpl c w i [DensePolynomial [1]]
  where i = 1
        b = derivative a
        c = primitiveEuclidian a b
        (w, _) = divmod a c

squareFreeFactorizationImpl :: RealFrac a => DensePolynomial a -> DensePolynomial a -> Integer -> [DensePolynomial a] -> [DensePolynomial a]
squareFreeFactorizationImpl (DensePolynomial [1.0]) w i solution = DensePolynomial.exp w i : solution

squareFreeFactorizationImpl poly@(DensePolynomial [c]) w i solution = squareFreeFactorizationImpl newC w i newSolution
  where newC = DensePolynomial [1.0]
        newSolution = poly : solution

squareFreeFactorizationImpl c w i solution = squareFreeFactorizationImpl cNew y (i + 1) newSolution
  where y = primitiveEuclidian w c
        (z, _) = divmod w y
        newSolution = DensePolynomial.exp z i : solution
        (cNew, _) = divmod c y



















replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n newVal xs = before ++ (newVal : after)
  where (before, _:after) = splitAt n xs

round' :: (Fractional a, RealFrac a) =>  Integer -> a -> a
round' sg num = (fromIntegral . round $ num * f) / f
    where f = 10^sg







