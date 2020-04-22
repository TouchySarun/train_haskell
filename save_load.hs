module Save_Load
    where

import Table_status

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