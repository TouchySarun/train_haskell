module SPR_Module
    where

import System.Random

random_spr = do
   randomIndex <- randomRIO (0,2)
   return(['s','s','s'] !! randomIndex)

spr x y 
    | x == y = "draw"
    | (x == 's' && y == 'p') || (x == 'p' && y == 'r') || (x == 'r' && y == 's') = "win"
    | otherwise = "lose"
