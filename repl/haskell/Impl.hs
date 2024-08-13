import Control.Monad (replicateM_)

solve :: [Int] -> [Int]
solve = reverse

main :: IO ()
main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        n <- readLn :: IO Int
        p <- map read . words <$> getLine
        putStrLn $ unwords $ map show $ solve p
