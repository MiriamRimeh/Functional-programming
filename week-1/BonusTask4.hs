main ::IO()
main = do
   print $ myGcd 5 13 == 1
   print $ myGcd 13 1235 == 13

myGcd :: Int -> Int -> Int
myGcd 0 y = y 
myGcd x 0 = x 
myGcd x y
 | x < 0 = error "x was negative"
 | y < 0 = error "y was negative"
 | otherwise = myGcd y (mod x y)
