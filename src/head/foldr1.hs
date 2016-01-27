myHeadFoldr1 :: [a] -> a
myHeadFoldr1 xs = foldr1 (\x _ -> x) xs
