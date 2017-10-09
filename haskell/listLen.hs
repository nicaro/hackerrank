len :: [a] -> Int
len lst =
    let co (l:ls) n = co ls (n+1)
        co [] n = n
    in co lst 0
