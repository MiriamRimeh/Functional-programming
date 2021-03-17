main ::IO()
main = do
   print $ rev 1 == 1
   print $ rev 123 == 321
   print $ rev 987654321 == 123456789

rev :: Int -> Int
rev x
 | x < 0 = error "Number can't be negative"
 | otherwise = helper x 0
  where 
     helper :: Int -> Int -> Int
     helper leftover result
      |leftover < 10 = result * 10 + leftover
      | otherwise = helper (div leftover 10) (result * 10 + mod leftover 10)