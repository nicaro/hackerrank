f :: [Int] -> [Int]
f lst = 
    let a = indexed lst
        b = filter (\(x,_) -> odd x) a
    in snd $ unzip b

indexed :: [Int] -> [(Int, Int)]
indexed xs = go 0 xs
   where
     go i (a:as) = (i, a) : go (i + 1) as
     go _ _ = []
