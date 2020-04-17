import System.Random

random_spr = do
   randomIndex <- randomRIO (0,2)
   print $ ['S','P','R'] !! randomIndex

spr x y 
    | x == y = "draw"
    | (x == 'S' && y == 'P') || (x == 'P' && y == 'R') || (x == 'R' && y == 'S') = "win"
    | otherwise = "lose"
