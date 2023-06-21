module Main where 

import DensePolynomial
import Test.HUnit
import qualified System.Exit as Exit


poly11 :: DensePolynomial Integer
poly11 = DensePolynomial [1, 1, 0]
poly12 :: DensePolynomial Integer
poly12 = DensePolynomial [1, -1, -1]
poly13 :: DensePolynomial Integer
poly13 = DensePolynomial [2, 0, -1]
sumTestEqualLength :: Test 
sumTestEqualLength = TestCase (assertEqual "should return [2,0, -1]" poly13 (add poly11 poly12))

poly21 :: DensePolynomial Integer
poly21 = DensePolynomial [2,0,1]
poly22 :: DensePolynomial Integer
poly22 = DensePolynomial [-1, 1]
poly23 :: DensePolynomial Integer
poly23 = DensePolynomial [2, -1, 2]

sumTestFirstLonger :: Test
sumTestFirstLonger = TestCase (assertEqual "should return [2, -1, 2]" poly23 (add poly21 poly22))

sumTestSecondLonger :: Test
sumTestSecondLonger = TestCase (assertEqual "should return [2, -1, 2]" poly23 (add poly22 poly21))

poly31 :: DensePolynomial Integer
poly31 = DensePolynomial []
poly32 :: DensePolynomial Integer
poly32 = DensePolynomial [1, -1]

sumTestFirstNull :: Test
sumTestFirstNull = TestCase (assertEqual "should return [1, -1]" poly32 (add poly31 poly32))
sumTestSecondNull = TestCase (assertEqual "should return [1, -1]" poly32 (add poly32 poly31))

sumTestNullNull = TestCase (assertEqual "should return []" poly31 (add poly31 poly31))

poly41 :: DensePolynomial Double
poly41 = DensePolynomial [2.0, -1.0, 0.0]
poly42 :: DensePolynomial Double
poly42 = DensePolynomial [2.0, -1.0]
poly43 :: DensePolynomial Double
poly43 = DensePolynomial [2.0, 1.0, -1.0]

sumTestDouble :: Test
sumTestDouble = TestCase (assertEqual "should return [2.0, 1.0, -1.0]" poly43 (add poly41 poly42))

poly51 :: DensePolynomial Double
poly51 = DensePolynomial [2.0, 1.0, 0.0]
poly52 :: DensePolynomial Double
poly52 = DensePolynomial [-2.0, 1.0]
poly53 :: DensePolynomial Double
poly53 = DensePolynomial [2.0, 3.0, -1.0]
poly54 :: DensePolynomial Double
poly54 = DensePolynomial [-2.0, -3.0, 1.0]

subTestFirstLonger :: Test
subTestFirstLonger = TestCase (assertEqual "should return [2.0, 3.0, -1.0]" poly53 (sub poly51 poly52))

subTestSecondLonger :: Test
subTestSecondLonger = TestCase (assertEqual "should return [-2.0, -3.0, 1.0]" poly54 (sub poly52 poly51))

poly61 :: DensePolynomial Double
poly61 = DensePolynomial [2.0, 1.0]
poly62 :: DensePolynomial a
poly62 = DensePolynomial []
poly63 :: DensePolynomial Double
poly63 = DensePolynomial [-2.0, -1.0]

subTestFirstNull :: Test
subTestFirstNull = TestCase (assertEqual "should return [-2.0, -1.0]" poly63 (sub poly62 poly61))
subTestSecondNull :: Test
subTestSecondNull = TestCase (assertEqual "should return [2.0, 1.0]" poly61 (sub poly61 poly62))

subTestBothNull :: Test
subTestBothNull = TestCase(assertEqual "should return []" poly62 (sub poly62 poly62))

poly71 = DensePolynomial [1.0, 1.0]
poly72 = DensePolynomial [1, 1]

poly73 = DensePolynomial [1.0, 2.0, 1.0]
poly74 = DensePolynomial [1, 2, 1]

mulTestEqualLength = TestCase(assertEqual "should return [1, 2, 1]" poly74 (mul poly72 poly72))



tests :: Test
tests = TestList [TestLabel "sumTestEqualLength" sumTestEqualLength,
                  TestLabel "sumTestFirstLonger" sumTestFirstLonger,
                  TestLabel "sumTestSecondLonger" sumTestSecondLonger,
                  TestLabel "sumTestFirstNull" sumTestFirstNull,
                  TestLabel "sumTestSecondNull" sumTestSecondNull,
                  TestLabel "sumTestNullNull" sumTestNullNull,
                  TestLabel "sumTestDouble" sumTestDouble,
                  TestLabel "subTestFirstLonger" subTestFirstLonger,
                  TestLabel "subTestSecondLonger" subTestSecondLonger,
                  TestLabel "subTestFirstNull" subTestFirstNull,
                  TestLabel "subTestSecondNull" subTestSecondNull,
                  TestLabel "subTestBothNull" subTestBothNull,
                  TestLabel "mulTestEqualLength" mulTestEqualLength]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess