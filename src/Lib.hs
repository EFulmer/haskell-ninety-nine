module Lib where

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

problemFour :: [a] -> Int
problemFour xs = go xs 0
  where go []     n = n
        go (x:xs) n = go xs (succ n)

problemFive :: [a] -> [a]
problemFive []     = []
problemFive [x]    = [x]
problemFive (x:xs) = (problemFive xs) ++ [x]

problemSix :: (Eq a) => [a] -> Bool
problemSix xs = xs == problemFive xs

data NestedList a = Elem a | List [NestedList a] deriving (Show)

problemSeven :: NestedList a -> [a]
problemSeven nl = case nl of
  Elem a  -> [a]
  List [] -> []
  List as -> concatMap problemSeven as

problemEight :: (Eq a) => [a] -> [a]
problemEight [] = []
problemEight (x:xs) = x : (problemEight (myDropWhile (==x) xs))
  where 
    myDropWhile p xs
      | null xs   = []
      | otherwise = if p (head xs) then myDropWhile p (tail xs) else xs

problemNine :: (Eq a) => [a] -> [[a]]
problemNine [] = []
problemNine l@(x:xs) = xRun : (problemNine rest)
  where 
    xRun = takeWhile (==x) l
    rest = dropWhile (==x) l

problemTen :: (Eq a) => [a] -> [(Int, a)]
problemTen xs = fmap (\x -> (length x, head x)) (problemNine xs)
