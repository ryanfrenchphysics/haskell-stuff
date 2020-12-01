import System.Random
import System.IO
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStr "Which number in the range 1-10 am I thinking of? "
    hFlush stdout
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = reads numberString
        if randNumber == number
            then putStrLn "Correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        askForNumber newGen