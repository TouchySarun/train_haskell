import Control.Monad
import Data.List

show_Gameover :: IO ()
show_Gameover = do
    putStrLn "                    "
    putStrLn "                    "
    putStrLn "                    "
    putStrLn "     Game  Over     "
    putStrLn "                    "
    putStrLn "                    "
    putStrLn "                    "
    putStrLn "--------------------"

show_table :: String -> IO ()
show_table [] = show_Gameover
show_table tb = 
    where
        x = tb !! 0
        y = tb !! 1
        black = find_range (tb !! 2) (tb !! 3) (tb !! 8) ""
        red = find_range (tb !! 4) (tb !! 5) (tb !! 8) ""

find_range :: (Num a, Eq a, Show a) => a -> a -> a -> [Char] -> [Char]
find_range h t s r 
    | h == t = r
    | h == s = find_range 1 t s (r ++ "," ++show h)
    | otherwise = find_range (h+1) t s (r ++ "," ++ show h)

show_bit :: (Num a, Show a) => a -> [Char] -> [Char] -> [Char]
show_bit index red black
    | isInfixOf (show index) red = "* "
    | isInfixOf (show index) black = "+ "
    | otherwise = ". "

show_row tbx tby x y 
    | x == 0 = 