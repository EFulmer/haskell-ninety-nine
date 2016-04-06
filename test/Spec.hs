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
prop_PoblemThree xs k = (problemThree xs k) == (atMay xs (pred k))

main :: IO ()
main = quickCheck $ \xs -> prop_ProblemOne (xs :: [Int])
