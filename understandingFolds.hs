foldl (flip (*)) 1 [1..3]
foldl (flip (*)) 1 (1 : 2 : 3 : [])
(((1 *  2) * 3) * 1)


foldl ((++ . show) "" [1..5]

foldr ((++ . show) "" [1..5]
