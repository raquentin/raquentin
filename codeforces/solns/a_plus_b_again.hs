import Control.Monad (replicateM)
 
processNums :: [String] -> [Int]
processNums = map sumDigits where
    sumDigits :: String -> Int
    sumDigits = sum . map (\c -> read [c] :: Int)
 
main :: IO ()
main = do
    n <- readLn :: IO Int
    nums <- replicateM n getLine
    mapM_ print $ processNums nums
