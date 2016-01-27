myFilterLC :: (a -> Bool) -> [a] -> [a]
myFilterLC f xs = [ x | x <- xs, f x ]
