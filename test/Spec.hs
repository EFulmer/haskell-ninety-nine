module Spec
  (
  main
  ) where

import Safe            (atMay, initMay, lastMay)

import Test.QuickCheck

import Lib

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

main :: IO ()
main = do
  quickCheck $ \xs -> prop_ProblemOne   (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemTwo   (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemThree (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemFour  (xs :: [Int])
  quickCheck $ \xs -> prop_ProblemFive  (xs :: [Int])
