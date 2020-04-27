
import Get_input
import SPR_Module
import Save_Load
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
    else
        return()
    let loop = do
        getchoice <- get_choice
        randomSPR <- random_spr
        let ck = spr getchoice randomSPR
        print randomSPR
        case ck of
            "lose" -> loop
            "draw" -> loop
            "win" -> print "End Game"
    loop  -- start the first iteration
