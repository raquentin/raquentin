import Data.Maybe (isJust)

canSayHello :: String -> Bool
canSayHello s = isJust $ checkSubseq "hello" s
    where
        checkSubseq :: String -> String -> Maybe ()
        checkSubseq [] _ = Just ()
        checkSubseq _ [] = Nothing
        checkSubseq (x:xs) (y:ys)
            | x == y = checkSubseq xs ys
            | otherwise = checkSubseq (x:xs) ys

main :: IO ()
main = do
    s <- getLine
    if canSayHello s
        then putStrLn "YES"
        else putStrLn "NO"
