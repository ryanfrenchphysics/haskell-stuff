import Control.Monad
import Data.Char

main = forever $ do
    putStr "Input: "
    l <- getLine
    putStrLn $ map toUpper l