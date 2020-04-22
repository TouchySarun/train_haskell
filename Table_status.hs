module Table_status
    where

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