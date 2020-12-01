import Data.Char
import Data.List

main :: IO ()
main = do
    -- Here we use fmap and function composition to:
    -- 1) Take in string
    -- 2) Uppercase it
    -- 3) Reverse it
    -- 4) Intersperse hyphens
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line

