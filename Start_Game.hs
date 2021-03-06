import Get_input
import SPR_Module
import Save_Load
import Show
import Table_status

import System.IO 

-- game_over = 1 return true else return false
-- gameover table = True
gameover table
    | (head_black table) == (head_red table) = True
    | (head_black table) == 0 = True
    | (head_red table) == 0 = True
    | otherwise = False

--gen_table _ _ table = table
gen_table length high = Table_status (length) (high) (length + high - 1) ((2*length) + high - 2) (1) (length) (0) (0) ((2*length) + (2*high) - 4)


start_game = do
   
    --Load File
    readHandle <- openFile "save.txt" ReadMode
    contents <- hGetContents readHandle
    seq contents (return ())

    let new = words contents
    let newer = map (\x -> read x::Int) new
    let table_status = Table_status (newer!!0) (newer!!1) (newer!!2) (newer!!3) (newer!!4) (newer!!5) (newer!!6) (newer!!7) (newer!!8)

    --hClose readHandle
    --End Load

    if gameover table_status 
    then 
        do 
        print "Gen Game"
        hClose readHandle --Close File Before

        getlong <- get_long
        gethigh <- get_high 
        let table_status = gen_table getlong gethigh
        show_table table_status

        --Save File
        writeHandle <- openFile "save.txt" WriteMode
        hPutStr writeHandle (normalize_save table_status)
        hClose writeHandle
        --End Save
    else
        return()
    hClose readHandle --Close File Before
    let loop = do

        --Load File
        readHandle <- openFile "save.txt" ReadMode
        contents <- hGetContents readHandle
        seq contents (return ())

        let new = words contents
        let newer = map (\x -> read x::Int) new
        let table_status = Table_status (newer!!0) (newer!!1) (newer!!2) (newer!!3) (newer!!4) (newer!!5) (newer!!6) (newer!!7) (newer!!8)

        --End Load

        getchoice <- get_choice
        randomSPR <- random_spr
        let ck = spr getchoice randomSPR
        print randomSPR
        -- let tb <- table_status
        let ts = update_table ck table_status

        let table_status' = edit_if_over ts ck (gameover ts)

        show_table table_status'

        --Save File
        seq contents (writeFile "save.txt" (normalize_save table_status'))
        hClose readHandle
        --End Save
        
        case gameover table_status' of
            False -> loop
            True -> show_Gameover
    loop


normalize_save status = show (table_size_x status) ++ "\n" ++ show (table_size_y status) ++ "\n" ++ show (head_black status)
                        ++ "\n" ++ show (tail_black status) ++ "\n" ++ show (head_red status) ++ "\n" ++ show (tail_red status)
                        ++ "\n" ++ show (game_over status) ++ "\n" ++ show (turn status) ++ "\n" ++ show (size status)

update_table ck tb 
    | ck == "lose" = Table_status (table_size_x tb) (table_size_y tb) (head_black tb) (move_lose_tail (tail_black tb) (head_red tb) (size tb)) (move (head_red tb) (size tb)) (move_win_tail (tail_red tb) (head_red tb) (tail_black tb) (size tb)) (game_over tb) ((turn tb)+1) (size tb)
    | ck == "win" = Table_status (table_size_x tb) (table_size_y tb) (move (head_black tb) (size tb)) (move_win_tail (tail_black tb) (head_black tb) (tail_red tb) (size tb)) (head_red tb) (move_lose_tail (tail_red tb) (head_black tb) (size tb)) (game_over tb) ((turn tb+1)) (size tb)
    | ck == "draw" = Table_status (table_size_x tb) (table_size_y tb) (head_black tb) (tail_black tb) (head_red tb) (tail_red tb) (game_over tb) ((turn tb)+1) (size tb)

-- eaten ck tb
--     | ck == 

move current size
    | current == 1 = size
    | otherwise = current-1

move_win_tail current head tail size
    | move head size == tail = current
    | otherwise = move current size

-- move_lose_head current head tail size
--     | head+1 == tail = current
--     | otherwise = move current size

move_lose_tail current head size
    | move head size == current = move current size 
    | otherwise = current

edit_if_over :: Table_status -> [Char] -> Bool -> Table_status
edit_if_over ts ck False = ts
edit_if_over ts ck True 
    | ck == "win" = Table_status (table_size_x ts) (table_size_y ts) (head_black ts) (tail_black ts) (0) (0) (0) (turn ts) (size ts)
    | ck == "lose" = Table_status (table_size_x ts) (table_size_y ts) (0) (0) (head_red ts) (tail_red ts) (0) (turn ts) (size ts)