module Main where

import Text.ParserCombinators.Parsec
import Data.List

-- Assume we use standard 700 Mb CDs for now
mediaSize = 700*1024*1024

-- parse output of "du -sb"
parseInput = do
    dirs <- many dirAndSize
    eof :: Parser ()
    return dirs

-- Datatype Dir holds info about single directory - its size and name
data Dir = Dir
    {
        dirSize :: Int,
        dirName :: String
    } deriving (Show)

-- Datatype holding a set of directories
data DirPack = DirPack
    {
        packSize :: Int,
        dirs :: [Dir]
    } deriving (Show)

-- parse info about single directory
dirAndSize = do
    size <- many1 digit
    spaces
    dirName <- anyChar `manyTill` newline
    return (Dir (read size) dirName)


-- Greedy packer tries to add directories one-by-one to initially empty "DirPack"
greedyPack dirs = foldl maybeAddDir (DirPack 0 []) $ sortBy cmpSize dirs where
    cmpSize d1 d2 = compare (dirSize d1) (dirSize d2)

-- Helper function: only adds directory to pack when new total size does not exceed media size
maybeAddDir p d =
    let
        newSize = packSize p + dirSize d
        newDirs = d:(dirs p)
        in
            if newSize > mediaSize then p else DirPack newSize newDirs

main = do
    input <- getContents
    putStrLn $ "DEBUG: got input " ++ input
    let
        dirs = case parse parseInput "stdin" input of
            Left err -> error $ "Input:\n" ++ show input ++ "\nError:\n" ++ show err
            Right result -> result

    putStrLn "DEBUG: parsed:"
    print dirs

    putStrLn "\n\n"

    putStrLn "Solution:"
    print $ greedyPack dirs
