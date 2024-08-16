import Control.Monad (replicateM)
 
abbreviate :: String -> String
abbreviate word
    | length word > 10 = head word : show (length word - 2) ++ [last word]
    | otherwise = word
 
processWords :: [String] -> [String]
processWords words = map abbreviate words
 
main :: IO ()
main = do
    n <- readLn :: IO Int
    wordsList <- replicateM n getLine
    let results = processWords wordsList
    mapM_ putStrLn results
