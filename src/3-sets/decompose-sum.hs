dCompose :: Int -> [[Int]]
dCompose set_size = [ [a, b, c] | a <- range, b <- range, c <- range,  
                                  a + b + c == the_sum, 
                                  a /= b, b /= c, a /= c, 
                                  a < b, b < c,
                                  a >= t, b >= t, c >= t] 
                            where range = [1 .. the_sum]
                                  t = set_size - 1
                                  the_sum = set_size^3 - 1

dCompose2 :: Int -> Int -> Int -> [[[Int]]]
dCompose2 set_size seed1 seed2 = [ [a, b, c] | length a == length b, length a == set_size, 
                                               seed1 `elem` a,  seed2 `elem` b, 
                                               abs( head a - head b) < 3]