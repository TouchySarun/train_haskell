
import Get_input
import SPR_Module
import Save_Load
import Show
import Table_status

-- game_over = 1 return true else return false
-- gameover table = True
gameover table
    | (head_black table) == (head_red table) = True
    | otherwise = False

--gen_table _ _ table = table
gen_table length high = Table_status (length) (high) (length + high - 1) ((2*length) + high - 2) (1) (length) (0) (0) ((2*length) + (2*high) - 4)


start_game = do
    table_status <- load
    if gameover table_status 
    then 
        do
        getlong <- get_long
        gethigh <- get_high
        let table_status = gen_table getlong gethigh
        print "Gen Game"
        -- start_game
    else
        return()
    getchoice <- get_choice
    randomSPR <- random_spr
    let ck = spr getchoice randomSPR
    print randomSPR
        -- let tb <- table_status
    let table_status' = update_table ck table_status
    print table_status
    show_table table_status'
    save table_status'
    case gameover table_status' of
        False -> start_game
        True -> show_Gameover
   -- start the first iteration

update_table ck tb 
    | ck == "lose" = Table_status (table_size_x tb) (table_size_y tb) (head_black tb) (move_lose_tail (tail_black tb) (head_red tb) (size tb)) (move_win_head (head_red tb) (size tb)) (move_win_tail (tail_red tb) (head_red tb) (tail_black tb) (size tb)) (game_over tb) ((turn tb)+1) (size tb)
    | ck == "win" = Table_status (table_size_x tb) (table_size_y tb) (move_win_head (head_black tb) (size tb)) (move_win_tail (tail_black tb) (head_black tb) (tail_red tb) (size tb)) (head_red tb) (move_lose_tail (tail_red tb) (head_black tb) (size tb)) (game_over tb) ((turn tb+1)) (size tb)
    | ck == "draw" = Table_status (table_size_x tb) (table_size_y tb) (head_black tb) (tail_black tb) (head_red tb) (tail_red tb) (game_over tb) ((turn tb)+1) (size tb)

-- eaten ck tb
--     | ck == 

move_win_head current size
    | current == 1 = size
    | otherwise = current-1

move_win_tail current head tail size
    | head-1 == tail = current
    | otherwise = move_win_head current size

-- move_lose_head current head tail size
--     | head+1 == tail = current
--     | otherwise = move_win_head current size

move_lose_tail current head size
    | head-1 == current = move_win_head current size
    | otherwise = current