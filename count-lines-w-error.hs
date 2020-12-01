import System.Environment ( getArgs )
import System.IO ()
import Control.Exception ( catch )
import System.IO.Error ( isDoesNotExistError )

main = toTry `catch` handler

toTry :: IO ()
toTry = do 
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "This file does not exist!"
    | otherwise = ioError e