module NumberUtils where


gcd :: Integer -> Integer -> Integer
gcd a b = q
  where (q, _ ,_) = _extendedEuclidian (abs a) (abs b) 1 0 0 1

gcdExtended :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExtended a b = _extendedEuclidian (abs a) (abs b) 1 0 0 1

_extendedEuclidian :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer) 
_extendedEuclidian c d c1 c2 d1 d2 
  | d == 0 = (abs c, c1, c2) 
  | otherwise = _extendedEuclidian d r d1 d2 r1 r2
    where q = c `div` d 
          r = c - q * d 
          r1 = c1 - q * d1 
          r2 = c2 - q * d2 