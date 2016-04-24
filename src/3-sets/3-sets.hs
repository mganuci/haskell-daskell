neighbours :: Int -> Int -> [Int]
neighbours seed range = [ y | y <- [seed - range .. seed + range], y /= seed ]

combinations :: Int -> [Int] -> [[Int]]
combinations _ [] = [[]]
combinations 0 _  = [[]]
combinations k (x:xs) = x_start ++ others
    where x_start = [ x : rest | rest <- combinations (k-1) xs ]
          others  = if k <= length xs then combinations k xs else []

maxRange :: Int -> Int
maxRange r = r ^ 3

findSolutions :: Int -> Int -> Int -> Int -> [([Int], [Int], [Int])]
findSolutions seed1 seed2 seed3 set_size = filter (\x -> isSumSurj x (maxRange set_size)) (findCandidates seed1 seed2 seed3 set_size)

findCandidates :: Int -> Int -> Int -> Int -> [([Int], [Int], [Int])]
findCandidates seed1 seed2 seed3 set_size = [ (a, b, c) |
                                                       a <- map (seed1:) (combinations (set_size-1) (neighbours seed1 max)), 
                                                       b <- map (seed2:) (combinations (set_size-1) (neighbours seed2 max)), 
                                                       c <- map (seed3:) (combinations (set_size-1) (neighbours seed3 max)),
                                                       sum a + sum b + sum c == expectedTotalSum set_size,
                                                       disjunct a b,
                                                       disjunct b c,
                                                       disjunct a c 
                                            ]

                                            where
                                                       max = maxRange set_size

expectedTotalSum :: Int -> Int
expectedTotalSum a = (a * (a ^ 3 + 1)) `div` 2

isSumSurj :: ([Int], [Int], [Int]) -> Int -> Bool
isSumSurj a maxRange = exact allSums [1 .. maxRange]
                       where
                        allSums = [ x + y + z | x <- first a, y <- second a, z <- third a]


disjunct :: [Int] -> [Int] -> Bool
disjunct xs ys = (all (\x -> not (x `elem` ys)) xs) && (all (\y -> not (y `elem` xs)) ys)

exact :: [Int] -> [Int] -> Bool
exact xs ys = (all (\x -> x `elem` ys) xs) && (all (\y -> y `elem` xs) ys)


first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  