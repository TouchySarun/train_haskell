import System.Random

random_spr = do
   randomIndex <- randomRIO (0,2)
   print $ ['s','p','r'] !! randomIndex

spr x y 
    | x == y = "draw"
    | (x == 's' && y == 'p') || (x == 'p' && y == 'r') || (x == 'r' && y == 's') = "win"
    | otherwise = "lose"
