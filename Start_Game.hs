
import Get_input
import SPR_Module

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


load_file = Table_status 0 1 2 3 4 5 6 7 8 

gameover table = False

gen_table _ _ table = table


start_game = do
    let table_status = load_file
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
