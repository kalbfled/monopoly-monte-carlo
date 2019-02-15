-- (C) David J. Kalbfleisch 2019

-- Simulate a given number of Monopoly games with a given number of players,
-- and output the number and percentage of times each player wins.

-- Build: stack ghc -- -dynamic Main.hs -o monopoly

import Monopoly
import System.Environment
import System.Random


main = do
    args <- getArgs

    if length args < 2
        then do
            program_name <- getProgName
            putStrLn ("Usage: " ++ program_name ++ " <2-6 players> <number of games>")
        else do
            -- Consider the first two arguments; ignore the rest, if any.
            let (num_players:num_games:_) = args

            -- Create an infinite list of random integers in [1,6] to simulate rolling dice.
            generator <- getStdGen
            let dice = randomRs (1, 6) generator

            -- Run the simulation, and output the results.
            let num_games_int = read num_games :: Int
                outcome = simulateMonopoly (read num_players :: Int) num_games_int dice
            print outcome
            print [(fromIntegral wins) / (fromIntegral num_games_int) * 100 | wins <- outcome]  -- Display percentages.

