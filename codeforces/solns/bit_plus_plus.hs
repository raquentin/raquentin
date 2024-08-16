import Control.Monad (replicateM)
 
interpretLang :: [String] -> Int
interpretLang stmts = sum $ map interpretStmt stmts
 
interpretStmt :: String -> Int
interpretStmt stmt = case stmt of
    "++X" -> 1
    "X++" -> 1
    "--X" -> -1
    "X--" -> -1
    _ -> 0
 
main :: IO ()
main = do
    n <- readLn :: IO Int
    stmts <- replicateM n getLine
    print $ interpretLang stmts
