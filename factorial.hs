factOne :: Integer -> Integer
factOne n = if n==0 then 1 else n * factOne(n-1)

factTwo :: Integer -> Integer
factTwo 0 = 1
factTwo n = n * factTwo(n-1)

factThree :: Integer -> Integer
factThree x
  | x > 1 = x * factThree(x-1)
  | otherwise = 1

factFour :: Integer -> Integer
factFour n = product [1..n]
