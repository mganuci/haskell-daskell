myHeadPatternMatching :: [a] -> a
myHeadPatternMatching [] = error "No head - the list is empty - pattern matching"
myHeadPatternMatching (x:_) = x
