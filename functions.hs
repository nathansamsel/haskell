applyTwice :: (a->a)->a->a
applyTwice f x = f ( f x )

zipwith' :: (a->b->c)->[a]->[b]->[c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs)(y:ys) = f x y : zipwith' f xs ys

map' :: (a->b)->[a]->[b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

largest :: Int
largest = head (filter p [100000, 99999..]) where p x = x `mod` 3829 == 0

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
