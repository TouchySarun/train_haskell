
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
        do
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
