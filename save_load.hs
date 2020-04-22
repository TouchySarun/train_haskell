import Control.Monad (when)
import System.IO
import Data.Char

data Table_status = Table_status { 
    table_size_x :: Int ,
    table_size_y :: Int ,
    head_black :: Int ,
    tail_black :: Int ,
    head_red :: Int ,
    tail_red :: Int ,
    game_over :: Int ,
    turn :: Int ,
    size :: Int 
    }deriving (Show)

temp = Table_status 0 1 2 3 4 5 6 7 8 

save status = do
    let string_save = show (table_size_x status) ++ "\n" ++ show (table_size_y status) ++ "\n" ++ show (head_black status)
                    ++ "\n" ++ show (tail_black status) ++ "\n" ++ show (head_red status) ++ "\n" ++ show (tail_red status)
                    ++ "\n" ++ show (game_over status) ++ "\n" ++ show (turn status) ++ "\n" ++ show (size status)

    writeFile "save.txt" string_save

load = do
    contents <- readFile "save.txt"
    let new = words contents
    let newer = map (\x -> read x::Int) new
    let table_temp = Table_status (newer!!0) (newer!!1) (newer!!2) (newer!!3) (newer!!4) (newer!!5) (newer!!6) (newer!!7) (newer!!8)
    return(table_temp)
