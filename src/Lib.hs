module Lib
    ( someFunc
    , problemOne
    , problemTwo
    , problemThree
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

problemOne :: [a] -> Maybe a
problemOne []     = Nothing
problemOne (x:[]) = Just x
problemOne (x:xs) = problemOne xs

problemTwo :: [a] -> Maybe a
problemTwo []       = Nothing
problemTwo [x]      = Nothing
problemTwo (x:y:[]) = Just x
problemTwo (x:xs)   = problemTwo xs

problemThree :: [a] -> Int -> Maybe a
problemThree [] k     = Nothing
problemThree (x:xs) 1 = Just x
problemThree (x:xs) k = problemThree xs (pred k)
