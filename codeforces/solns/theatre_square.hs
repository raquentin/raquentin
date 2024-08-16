import Control.Monad (replicateM)
 
calculateFlagstones :: Integer -> Integer -> Integer -> Integer
calculateFlagstones n m k = tilesAlongN * tilesAlongM
    where
        tilesAlongN = (n + k - 1) `div` k
        tilesAlongM = (m + k - 1) `div` k
 
main :: IO ()
main = do
    input <- getLine
    let [n, m, k] = map read (words input) :: [Integer]
    print $ calculateFlagstones n m k
