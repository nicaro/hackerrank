import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double
        print $ roundToDec 4 $ tex x


getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

t0 :: Double -> Double
t0 x = 1.0 
t1 :: Double -> Double
t1 x = x 
t2 :: Double -> Double
t2 x = x**2 / fac 2 
t3 :: Double -> Double
t3 x = x**3 / fac 3
t4 :: Double -> Double
t4 x = x**4 / fac 4
t5 :: Double -> Double
t5 x = x**5 / fac 5
t6 :: Double -> Double
t6 x = x**6 / fac 6
t7 :: Double -> Double
t7 x = x**7 / fac 7
t8 :: Double -> Double
t8 x = x**8 / fac 8
t9 :: Double -> Double
t9 x = x**9 / fac 9

ta :: [Double -> Double]
ta = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]

tex :: Double -> Double
tex x = sum $ map ($ x) ta

fac :: Double -> Double
fac 0 = 1.0
fac n = n * fac (n-1)

roundToDec n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)