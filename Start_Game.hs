
import Get_input
import SPR_Module
import Save_Load
import Table_status

gameover table = True

gen_table _ _ table = table


start_game = do
    table_status <- load
    if gameover table_status 
    then 
        do
        getlong <- get_long
        gethigh <- get_high
        let table_status = gen_table get_long get_high table_status
        print "Gen Game"
    else
        return()
    let loop = do
        getchoice <- get_choice
        randomSPR <- random_spr
        let ck = spr getchoice randomSPR
        print randomSPR
        -- update_table ck table_status
        case ck of
            "lose" -> loop
            "draw" -> loop
            "win" -> print "End Game"
    loop  -- start the first iteration

update_table ck tb 
    | ck == "lose" = Table_status (table_size_x tb) (table_size_y tb) (head_black tb) (tail_black tb) (move (head_red tb) tb) (move (tail_red tb) tb) (game_over tb) ((turn tb)+1) (size tb)
    | ck == "win" = Table_status ((table_size_x tb)) (table_size_y tb) (move (head_black tb) tb) (move (tail_black tb) tb) (head_red tb) (tail_red tb) (game_over tb) ((turn tb+1)) (size tb)
    | ck == "draw" = Table_status ((table_size_x tb)) (table_size_y tb) (head_black tb) (tail_black tb) (head_red tb) (tail_red tb) (game_over tb) ((turn tb)+1) (size tb)

move current tb
    | current == 1 = size tb
    | otherwise = current-1
