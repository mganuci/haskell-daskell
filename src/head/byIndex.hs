myHeadByIndex :: [a] -> a
myHeadByIndex [] = error "Empty list - byIndex"
myHeadByIndex a = a !! 0
