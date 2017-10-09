f :: Int -> [Int] -> [Int]
f n arr = fi (<n) arr

fi :: (a -> Bool) -> [a] -> [a]
fi _ [] = []
fi p (x:xs)
 | p x = x : fi p xs
 | otherwise = fi p xs
