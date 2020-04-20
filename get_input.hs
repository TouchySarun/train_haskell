module Get_input
    where


import Control.Monad
import Data.Typeable
import Control.Exception

-- ....

readInt :: String -> Int
readInt a = read a

get_choice = do
    -- we define "loop" as a recursive IO action
    let loop = do
        putStrLn "Enter your choice : s(scissors), p(paper), r(rock) : "
        choice <- getLine
        if choice == "s" || choice == "p" || choice == "r"
            then return(choice!!0)
            else loop
    loop  -- start the first iteration


get_long = do
    -- we define "loop" as a recursive IO action
    let loop = do
        putStrLn "Enter table long (min 2, max 10) : "
        input <- getLine
        eVal <- try ( print(readInt input)):: IO (Either SomeException ())
        case eVal of
            Left e -> loop
            Right n -> return(readInt input)
    loop  -- start the first iteration

get_high = do
    -- we define "loop" as a recursive IO action
    let loop = do
        putStrLn "Enter table high (min 2, max 10) : "
        input <- getLine
        eVal <- try ( print(readInt input)):: IO (Either SomeException ())
        case eVal of
            Left e -> loop
            Right n -> return(readInt input)
    loop  -- start the first iteration