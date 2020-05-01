module Show 
    where
import Control.Monad
import Table_status
import Data.List

show_Gameover :: IO ()
show_Gameover = do
    putStrLn "                                                  "
    putStrLn "             ####     #     #   # ####            "
    putStrLn "            #        ##    ##  ## #               "
    putStrLn "            #  ##   # #   # # # # ####            "
    putStrLn "            #   #  ####  #  ##  # #               "
    putStrLn "             ###  #   # #   #   # ####            "
    putStrLn "                                                  "
    putStrLn "                                                  "
    putStrLn "             ###  #   # #### ####                 "
    putStrLn "            #   # #  #  #    #   #                "
    putStrLn "            #   # # #   #### ####                 "
    putStrLn "            #   # ##    #    #  #                 "
    putStrLn "             ###  #     #### #   #                "
    putStrLn "                                                  "
    putStrLn "                                                  "

show_table tb = show_table' (table_size_y tb) tb

show_table' 0 _ = return ()
show_table' n tb =
    do
        putStrLn (show_row x y 1 (y-(n-1)) "" red black)
        if(n /= 1) 
            then 
                putStrLn (show_edge_row (x-1)) 
            else
                putStrLn (show_last_row (x))        
        show_table' (n-1) tb
    where
        x = table_size_x tb
        y = table_size_y tb
        red = find_range (head_black tb) (tail_black tb) (size tb) ""
        black = find_range (head_red tb) (tail_red tb) (size tb) ""

find_range :: (Num a, Eq a, Show a) => a -> a -> a -> [Char] -> [Char]
find_range h t s r 
    | h == t = r ++ "," ++ show h ++ ","
    | h == s = find_range 1 t s (r ++ "," ++show h)
    | otherwise = find_range (h+1) t s (r ++ "," ++ show h)


show_bit :: (Num a, Show a) => a -> [Char] -> [Char] -> [Char]
show_bit index red black
    | isInfixOf ("," ++ (show index) ++ ",") red = "  #  "
    | isInfixOf ("," ++ (show index) ++ ",") black = "  o  "
    | otherwise = "     "

show_row :: (Num a, Eq a, Show a) => a -> a -> a -> a -> [Char] -> [Char] -> [Char] -> [Char] 
show_row tbx tby x y r red black
    | y == 1 && x == tbx = 
        r ++ (show_bit x red black)

    | y == 1 && x /= tbx = 
        (show_row tbx tby (x+1) y (r ++ (show_bit x red black)) red black)

    | y == tby && x == tbx = 
        r ++ (show_bit (tbx + tby - 1) red black)

    | y == tby && x /= tbx = 
        (show_row tbx tby (x+1) y (r ++ (show_bit (((tbx*2) + tby - 1) - x) red black)) red black)

    | y /= 1 && y/= tby && x == 1 = 
        (show_row tbx tby (x+1) y (r ++ (show_bit ( ((((tbx+tby)*2) - 4) - y) + 2) red black)) red black)

    | y /= 1 && y/= tby && x == tbx = 
        r ++ (show_bit (tbx + y - 1) red black)

    | otherwise = 
        (show_row tbx tby (x+1) y (r ++ "     ") red black)

show_edge_row n = show_edge_row' n ""
    where 
        show_edge_row' 0 r = r
        show_edge_row' n r = show_edge_row' (n-1) (r++"    +")

show_last_row n = show_last_row' n ""
    where
        show_last_row' 0 r = r
        show_last_row' n r = show_last_row' (n-1) (r++"-----")