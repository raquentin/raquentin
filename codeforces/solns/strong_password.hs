import Control.Monad (replicateM)
 
processLine :: String -> String
processLine [] = []
processLine [x] = x : [diffChar x]
processLine (x:y:xs)
    | x == y    = x : diffChar x : y : xs
    | otherwise = x : processLine (y:xs)
 
diffChar :: Char -> Char
diffChar 'z' = 'a'
diffChar c = succ c
 
main :: IO ()
main = do
    n <- readLn :: IO Int
    lines <- replicateM n getLine
    mapM_ putStrLn $ processLine <$> lines
