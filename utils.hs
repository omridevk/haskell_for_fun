map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

type Test = Bool

filter' :: (a -> Test) -> [a] -> [a]
filter' _ []    = []
filter' p (x:xs)
    | p x       = x : filter' p xs 
    | otherwise = filter' p xs

filterOdd = filter' odd

filterOddSquared = map (\x -> x * x) . filter' (>100)