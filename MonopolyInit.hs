-- (C) David J. Kalbfleisch 2019

module MonopolyInit (
    initPlayers,
    initRealEstate,
) where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

type RealEstate = IntMap.IntMap Int
type Properties = IntSet.IntSet
type Player = (Int, Int, Properties)  -- position, money, properties
type Players = [Player]


go = 0
initialMoney = 1500  -- $1,500


initPlayers :: Int -> Players
-- Each player: (Go, initialMoney, no properties owned)
initPlayers num_players = (go, initialMoney * 2, IntSet.empty :: Properties):(replicate (num_players - 1) (go, initialMoney, IntSet.empty :: Properties))


initRealEstate :: RealEstate
-- This map simulates the board spaces that can be developed with houses and hotels.  The key is as in initBoard.
-- The value, initialized to 0, is an integer 0-5 denoting the number of houses on the lot (5 for a hotel).
initRealEstate = IntMap.fromList $ zip [1, 3, 6, 8, 9, 11, 13, 14, 16, 18, 19, 21, 23, 24, 26, 27, 29, 31, 32, 34, 37, 39] (repeat 0)

