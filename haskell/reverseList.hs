rev t = 
    let doReverse (l:ls) r = doReverse ls (l:r)
        doReverse [] r = r
    in doReverse t []
