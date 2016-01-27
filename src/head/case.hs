myHeadCase :: [a] -> a
myHeadCase xs = case xs of [] -> error "No head - the list is empty - case"
                           (x:_) -> x
