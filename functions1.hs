--applies sqrt to each element of a list
rootList :: [Double] -> [Double]
rootList xs = map sqrt xs

--multiplies the elements of tuples and returns them in list
--tuple product corresponds to index of original tuple
multiplyPairs :: [(Int, Int)] -> [Int]
multiplyPairs = map (\(x,y) -> x*y)

--increments the input list by the input increment
incrementList :: Int -> [Int] -> [Int]
incrementList z x = map (\x -> x+z) x

--sums the squares of each element from the input list
squareSum :: [Int] -> Int
squareSum (x:xs) = foldl (\acc x -> acc + (x * x)) x xs 

--executes a logical OR on the whole list
booleanOr :: [Bool] -> Bool
booleanOr [] = False 
booleanOr (x:xs) = foldl (\acc x -> acc || x) x xs

--executes a logical AND on the whole list
booleanAnd :: [Bool] -> Bool
booleanAnd [] = True
booleanAnd (x:xs) = foldl (\acc x -> acc && x) x xs

--duplicates each element of input list
dupList :: [a] -> [a]
dupList = (>>= replicate 2)

--counts occurrences of True in a list
--off by one
trueCount :: [Bool] -> Int
trueCount x = foldl (\acc x -> if x then acc+1 else acc) 0 x 

--rearranges triple-tuple in specified way
rotateTriples :: [(Int, Int, Int)] -> [(Int, Int, Int)]
rotateTriples = map (\(x,y,z) -> (y, z, x))

--flattens a list of lists of ints into a list of ints with same elements
concatLists :: [[Int]] -> [Int]
concatLists xss = foldl (++) [] xss 
