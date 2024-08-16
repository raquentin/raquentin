import Control.Monad (replicateM_)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
 
solve :: IO ()
solve = do
    input <- fmap (map read . words) getLine :: IO [Int]
    let [a1, a2, b1, b2] = input
    let answer = (if (a1 > b1 && a2 >= b2) || (a1 >= b1 && a2 > b2) then 2 else 0) +
                 (if (a2 > b1 && a1 >= b2) || (a2 >= b1 && a1 > b2) then 2 else 0)
    print answer
 
 
main :: IO ()
main = do
    t <- readLn :: IO Int
    replicateM_ t solve
