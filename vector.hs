addVec :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVec a b = ( fst a + fst b, snd a + snd b )

addVec2 :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVec2 (x,y) (a,b) = ( x + a, y + b )
