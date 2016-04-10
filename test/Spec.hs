module Spec
  (
  main
  ) where

import           Control.Monad      (liftM)
import qualified Data.Set      as S

import           Safe               (atMay, initMay, lastMay)
import           Test.QuickCheck

import           Lib

prop_ProblemOne :: (Eq a) => [a] -> Bool
prop_ProblemOne xs = problemOne xs == lastMay xs

prop_ProblemTwo :: (Eq a) => [a] -> Bool
prop_ProblemTwo xs = problemTwo xs == (initMay xs >>= lastMay)

prop_ProblemThree :: (Eq a) => [a] -> Int -> Bool
prop_ProblemThree xs k = (problemThree xs k) == (atMay xs (pred k))

prop_ProblemFour :: (Eq a) => [a] -> Bool
prop_ProblemFour xs = (problemFour xs) == (length xs)

prop_ProblemFive :: (Eq a) => [a] -> Bool
prop_ProblemFive xs = (problemFive xs) == (reverse xs)

prop_ProblemSix :: (Eq a) => [a] -> Bool
prop_ProblemSix xs = (problemSix xs) == (xs == reverse xs)

instance (Arbitrary a) => Arbitrary (NestedList a) where
  arbitrary = frequency
    [ (41, Elem <$> arbitrary)
    , (1, List <$> children) ]
    where children = sized $ \n -> vectorOf n arbitrary

prop_ProblemSeven :: (Eq a) => NestedList a -> Bool
prop_ProblemSeven xs = (length . problemSeven) xs == nlLength xs
  where 
    nlLength nl = go 0 nl
    go n (Elem _) = n + 1
    go n (List []) = n
    go n (List xs) = sum (fmap nlLength xs)

prop_ProblemEight :: (Ord a, Eq a) => [a] -> Bool
prop_ProblemEight xs = S.fromList (problemEight xs) == S.fromList xs

main :: IO ()
main = do
  quickCheck $ \xs -> prop_ProblemOne   (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemTwo   (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemThree (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemFour  (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemFive  (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemSix   (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemSeven (xs :: NestedList Int)
  quickCheck $ \xs -> prop_ProblemEight (xs :: [Int])
