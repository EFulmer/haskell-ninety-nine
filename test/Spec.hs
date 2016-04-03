module Spec
  (
  main
  ) where

import Safe (lastMay)

import Test.QuickCheck

import Lib

prop_ProblemOne :: (Eq a) => [a] -> Bool
prop_ProblemOne xs = problemOne xs == lastMay xs

main :: IO ()
main = quickCheck $ \xs -> prop_ProblemOne (xs :: [Int])
