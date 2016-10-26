import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String,Int)]
wordNums = map (\w -> (head w, length w)).group.sort.words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (needle `isPrefixOf`) (tails haystack)


