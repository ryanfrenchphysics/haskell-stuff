import System.IO

main = do
    putStrLn "Enter task to append to to-do list:"
    todoItem <- getLine
    appendFile "todo.txt" ("\n" ++ todoItem)