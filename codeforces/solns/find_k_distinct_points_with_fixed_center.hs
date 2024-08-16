import Control.Monad (replicateM)
 
solveTestCase :: Int -> Int -> Int -> [(Int, Int)]
solveTestCase xc yc k = 
    let points = [(xc + i, yc) | i <- [1..(k-1)]]
        xSum = sum (map fst points)
        xk = k * xc - xSum
    in points ++ [(xk, yc)]
 
solve :: [(Int, Int, Int)] -> [[(Int, Int)]]
solve = map (\(xc, yc, k) -> solveTestCase xc yc k)
 
main :: IO ()
main = do
    t <- readLn
    testCases <- replicateM t $ do
        [xc, yc, k] <- map read . words <$> getLine
        return (xc, yc, k)
    let results = solve testCases
    mapM_ (mapM_ (\(x, y) -> putStrLn $ show x ++ " " ++ show y)) results
