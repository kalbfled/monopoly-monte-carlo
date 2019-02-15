-- (C) David J. Kalbfleisch 2019

-- Simulate a given number of Monopoly games with a given number of players.

-- Some games are likely never to end.  Give the simulation of an individual game
-- a maximum number of turns to complete.  See:
--     https://www.informs-sim.org/wsc09papers/036.pdf

-- TODO - Multiprocessing

-- Rules and property values:
--     https://en.wikibooks.org/wiki/Monopoly/Official_Rules
--     https://www.hasbro.com/common/instruct/monins.pdf
--     https://researchmaniacs.com/Games/Monopoly/Properties.html

module Monopoly (
    developeRealEstate,
    divest,
    getPurchasePrice,
    getRentDue,
    incomeTax,
    initRealEstate,
    jail,
    justVisiting,
    luxuryTax,
    simulateMonopoly,
    takeTurn,
    tenPercent,
    unmortgageProperties,
    winnerIndex,
) where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import MonopolyInit

type RealEstate = IntMap.IntMap Int
type Properties = IntSet.IntSet
type Player = (Int, Int, Properties)  -- position, money, properties
type Players = [Player]

-- Constants; mostly numerical notations for board positions where "Go" is 0.
go = 0
goToJail = 30
incomeTax = 4
jail = 60            -- Larger than 39 (Boardwalk) + 12 (max die roll)
justVisiting = 10
luxuryTax = 38
turnsLimit = 20000   -- The maximum number of turns, per player (i.e. 40k limit for 2 players)

railroads = IntSet.fromList [5, 15, 25, 35]
utilities = IntSet.fromList [12, 28]
purple = IntSet.fromList [1, 3]
lightBlue = IntSet.fromList [6, 8, 9]
pink = IntSet.fromList [11, 13, 14]
orange = IntSet.fromList [16, 18, 19]
red = IntSet.fromList [21, 23, 24]
yellow = IntSet.fromList [26, 27, 29]
green = IntSet.fromList [31, 32, 34]
darkBlue = IntSet.fromList [37, 39]

board :: IntMap.IntMap [Int]
-- This map simulates the board spaces that can be purchased.  The key is an integer for each space, ordered
-- clockwise, where 0 denotes Go, 39 denotes Boardwalk, and 40 denotes Jail.  The value is an integer list of
-- [purchase price, vacant rent, 1 house rent, 2 houses rent, 3 houses rent, 4 houses rent, hotel rent].
-- Railroads and utilities only have a purchase price.
board = IntMap.fromList [
        (1, [60, 2, 10, 30, 90, 160, 250]),           -- Mediterranean Avenue
        (3, [80, 4, 20, 60, 180, 320, 450]),          -- Baltic Avenue
        (5, [200]),                                   -- Reading Railroad
        (6, [100, 6, 30, 90, 270, 400, 550]),         -- Central/Oriental Avenue
        (8, [100, 6, 30, 90, 270, 400, 550]),         -- Vermont Avenue
        (9, [120, 8, 40, 100, 300, 450, 600]),        -- Connecticut Avenue
        (11, [140, 10, 50, 150, 450, 625, 750]),      -- St. Charles Place
        (12, [150]),                                  -- Electric Company
        (13, [140, 10, 50, 150, 450, 625, 750]),      -- States Avenue
        (14, [160, 12, 60, 180, 500, 700, 900]),      -- Virginia Avenue
        (15, [200]),                                  -- Pennsylvania Railroad
        (16, [180, 14, 70, 200, 550, 750, 950]),      -- St. James Place
        (18, [180, 14, 70, 200, 550, 750, 950]),      -- Tennessee Avenue
        (19, [200, 16, 80, 220, 600, 800, 1000]),     -- New York Avenue
        (21, [220, 18, 90, 250, 700, 875, 1050]),     -- Kentucky Avenue
        (23, [220, 18, 90, 250, 700, 875, 1050]),     -- Indiana Avenue
        (24, [240, 20, 100, 300, 750, 925, 1100]),    -- Illinois Avenue
        (25, [200]),                                  -- B & O Railroad
        (26, [260, 22, 110, 330, 800, 975, 1150]),    -- Atlantic Avenue
        (27, [260, 22, 110, 330, 800, 975, 1150]),    -- Ventnor Avenue
        (28, [150]),                                  -- Water Works
        (29, [280, 24, 120, 360, 850, 1025, 1200]),   -- Marvin Gardens
        (31, [300, 26, 130, 390, 900, 1100, 1275]),   -- Pacific Avenue
        (32, [300, 26, 130, 390, 900, 1100, 1275]),   -- North Carolina Avenue
        (34, [320, 28, 150, 450, 1000, 1200, 1400]),  -- Pennsylvania Avenue
        (35, [200]),                                  -- Short Line Railroad
        (37, [350, 35, 175, 500, 1100, 1300, 1500]),  -- Park Place
        (39, [400, 50, 200, 600, 1400, 1700, 2000])   -- Boardwalk
    ]


simulateMonopoly :: Int -> Int -> [Int] -> [Int]
-- Given a number of players and games to play, return a list of the fraction of games won by each player.
simulateMonopoly num_players num_games dice
    | num_players < 2 || num_players > 6 || num_games < 1 = error "2-6 players must play at least 1 game."
    | otherwise = simulateMonopolyHelper num_players num_games dice (replicate num_players 0)


simulateMonopolyHelper :: Int -> Int -> [Int] -> [Int] -> [Int]
-- Return a list of integers denoting the number of times each player won.
simulateMonopolyHelper _ 0 _ tally = tally  -- Base case: There are no more games to play.
simulateMonopolyHelper num_players num_games dice tally =
    let (winner_index, dice') = playGame num_players dice
        winner_new_tally = (tally !! winner_index) + 1
        tally' = (take winner_index tally) ++ winner_new_tally:(drop (winner_index + 1) tally)
    in simulateMonopolyHelper num_players (num_games - 1) dice' tally'


playGame :: Int -> [Int] -> (Int, [Int])
-- Given the number of players and dice, simulate a single game.
-- Return the index of the winning player and a modified "dice" list.
playGame num_players dice = playGameHelper (initPlayers num_players) initRealEstate (IntSet.empty :: Properties) 0 dice (num_players * turnsLimit) 0


playGameHelper :: Players -> RealEstate -> Properties -> Int -> [Int] -> Int -> Int -> (Int, [Int])
-- Simulate a single game.  Return the index of the winning player and the modified "dice" list.
playGameHelper players real_estate mortgages player_index dice turns_limit turns_taken
    | turns_taken == turns_limit = playGame (length players) dice  -- This game seems like it won't end.  Start a new game with the modified dice list.
    | otherwise = case (winnerIndex players) of
                      Nothing -> let (players', real_estate', mortgages', dice') = takeTurn players real_estate mortgages player_index 0 dice
                                     player_index' = (player_index + 1) `mod` (length players)
                                 in playGameHelper players' real_estate' mortgages' player_index' dice' turns_limit (turns_taken + 1)
                      Just index -> (index, dice)


winnerIndex :: Players -> Maybe Int
-- Return the index of the winner, if any.  Otherwise, return Nothing.  There is a winner if only one player has non-negative money.
winnerIndex players
    | solvent_count == 1 = Just (head solvent_players)  -- We have a winner.
    | solvent_count > 1 = Nothing -- Multiple players are still in the game; no winner yet.
    where solvent_players = List.findIndices (\(_, money, _) -> money >= 0) players
          solvent_count = length solvent_players


takeTurn :: Players -> RealEstate -> Properties -> Int -> Int -> [Int] -> (Players, RealEstate, Properties, [Int])
-- Execute one ore more (doubles) turns for the current player, and return the updated game state: players, real estate, and dice.
takeTurn players real_estate mortgages current_player_index doubles_count dice
    | money < 0 = (players, real_estate, mortgages, dice)  -- No change in game state.  This player is bankrupt.
    | (doubles_count == 2) && (die1 == die2) =
        -- The player rolled doubles three times.  Go directly to jail.
        let players' = (take current_player_index players) ++ (jail, money, properties):(drop (current_player_index + 1) players)
        in (players', real_estate, mortgages, dice')
    | position == jail =
        -- The player is in jail.  Move the player to "just visiting", and otherwise skip the turn.  Simplify the rules by omitting the options of paying bail or rolling for doubles.
        -- TODO - Do not simplify the rules.  This requires a strategy, which might mean an AI approach.
        let players' = (take current_player_index players) ++ (justVisiting, money, properties):(drop (current_player_index + 1) players)
        in (players', real_estate, mortgages, dice)
    | otherwise =
        -- The player is not in jail.  Execute a normal turn, including rolling dice.
        let -- Update the player's position.
            position' = let raw_position = position + die1 + die2
                        in if raw_position == goToJail
                               then jail
                               else raw_position `mod` 40

            -- Who owns the property at the new position?  This has consequences for the player's money and property.
            landlord = List.findIndex (\(_, _, properties) -> IntSet.member position' properties) players  -- Maybe Int

            -- Update the player's money for passing Go, paying rent, and paying taxes, as applicable.  Do not buy property in this block.
            -- Note that going to jail does not pass Go because jail has the highest position value.
            passed_go_bonus = if (position' < position) then 200 else 0
            rent_owed = case landlord of
                            Nothing -> 0  -- Nobody owns the property.
                            Just landlord_index -> if IntSet.member position' mortgages
                                                       then 0  -- The property is mortgaged.  No rent is due.
                                                       else getRentDue real_estate current_player_index landlord_index position' players (die1 + die2)
            -- TODO - I want to use "case posiiton' of" instead of nested if/else, but it doesn't work for some reason with function names instead of literals.
            tax_owed = if position' == incomeTax
                           then min 200 (tenPercent real_estate current_player)
                           else if position' == luxuryTax
                                    then 100
                                    else 0
            money' = money + passed_go_bonus - rent_owed - tax_owed

            (real_estate', mortgages', money'') = if money' < 0
                                                      -- The player must divest, if possible.
                                                      then divest properties real_estate mortgages money'

                                                      -- The player has enough cash to cover current expenses.
                                                      else (real_estate, mortgages, money')
            -- If money'' is negative, the player is bankrupt.

            -- Update the player's property if it has at least $60, which is the price of the least expensive property.
            -- Note that updating the properties 
            purchase_price = if money'' >= 60
                                 then case landlord of
                                          Nothing -> case (getPurchasePrice position') of
                                                         Nothing -> 0  -- The property is not for sale because it is not ownable.
                                                         Just price -> if price <= money''
                                                                           then price -- The player has the money to buy the property.
                                                                           else 0     -- The player cannot afford to buy the property.  TODO - Auction the property.
                                          Just _ -> 0  -- Somebody owns the property already.  It is not for sale.
                                 else 0
            properties' = if purchase_price > 0
                               -- Purchase the property.
                               then IntSet.insert position' properties

                               else if money'' < 0
                                        -- The properties will transfer to the debtor.  Remove them from this player.
                                        then IntSet.empty :: Properties

                                        -- Do not purchase the property.
                                        else properties

            mortgages'' = if (money'' < 0) && (tax_owed > 0)
                              -- The player is bankrupt, and the bank is the debtor.  Unmortgage the properties, which become available for purchase again.
                              then IntSet.difference mortgages' properties  -- Not properties', which should be empty at this point of execution.

                              else mortgages'

            -- Update the player's money after buying property (not real estate), if applicable.
            money''' = money'' - purchase_price

            -- Unmortgage property, if possible.
            -- TODO - Don't hardcode the strategy of unmortgaging property before buying real esate.  This probably requires an AI approach.
            (mortgages''', money'''') = unmortgageProperties properties mortgages'' money'''

            -- If the player has at least $50 and has an unmortgaged monopoly on any property groups, buy as much real estate as possible for one monopolized group.
            -- TODO - Any player can purchase real estate at any time.
            -- TODO - If a player places a hotel on a position occupied by another player, rent is due immediately.
            (real_estate'', money''''') = let unmortgaged_properties = IntSet.difference properties' mortgages''
                                          in case (monopolizedColor unmortgaged_properties) of
                                                 Nothing -> (real_estate', money'''')  -- The player does not have a monopoly.
                                                 Just color -> developeRealEstate real_estate' color money''''

            players' = (take current_player_index players) ++ (position', money''''', properties'):(drop (current_player_index + 1) players)
            players'' = if rent_owed > 0
                            -- Rent was paid.  Update the landlord's money.
                            then case landlord of
                                     Nothing -> error "Rent was paid; there should be a landlord."
                                     Just landlord_index -> let (landlord_position, landlord_money, landlord_properties) = players' !! landlord_index
                                                                landlord_money' = landlord_money + (if (money''''' < 0) then 0 else rent_owed)  -- 0 for a bankrupt player.
                                                                landlord_properties' =
                                                                    if money''''' < 0
                                                                        -- The player is bankrupt, and a landlord assumes the bankrupt player's properties.
                                                                        -- TODO - This also transfers utilities and railroads, which cannot be mortgaged.
                                                                        then IntSet.union landlord_properties properties  -- Not properties', which should be empty at this point of execution.

                                                                        else landlord_properties
                                                            in (take landlord_index players') ++ (landlord_position, landlord_money', landlord_properties'):(drop (landlord_index + 1) players')
                            -- No rent was paid.
                            else players'

            -- TODO - Chance or Community Chest.  Draw a card, and execute the directions.  Ignore for now.
        in if (die1 == die2) && (money''''' >= 0)
               -- The player rolled doubles and is not bankrupt.  Take another turn.
               then takeTurn players'' real_estate'' mortgages''' current_player_index (doubles_count + 1) dice'

               -- The turn is over.
               else (players'', real_estate'', mortgages''', dice')
    where current_player = players !! current_player_index
          (position, money, properties) = current_player
          (die1:die2:dice') = dice


divest :: Properties -> RealEstate -> Properties -> Int -> (RealEstate, Properties, Int)
-- The player owes money and does not have adequate cash on hand.  Sell real estate first.  If the player has no
-- real estate to sell, mortgage properties.
-- TODO - Allow selling unimproved properties to other players at arbitrary prices.  This should include utilities and roilroads.
-- TODO - This algorithm is suboptimal.  A player with a $35 deficit who owns a house that can be sold for $25 and another house
-- that can be sold for $50 would sell both, but only the latter needs to be sold.  An associated test is testDivest3.
divest properties real_estate mortgages money
    | money >= 0 || IntSet.null properties =
        -- Base case: The player has enough money to cover current expenses or has no equity to divest.
        (real_estate, mortgages, money)
    | otherwise =
        -- The player still has negative money.  Continue divesting, if possible.
        let improved_real_estate = IntMap.filter (>0) (IntMap.restrictKeys real_estate properties)  -- Improved lots owned by this player
        in if IntMap.null improved_real_estate
               -- The player does not have any real estate to sell.  Mortgage properties, if possible.
               then let unmortgaged_properties = IntSet.difference properties mortgages
                    in if IntSet.null unmortgaged_properties
                           -- Base case: The player has no unmortgaged properties.
                           then (real_estate, mortgages, money)

                           -- The player must mortgage property.  The player must have at least one property to have avoided the base case (first guard).
                           else let property = IntSet.findMin unmortgaged_properties  -- Divest from the lowest rent properties first.
                                    mortgages' = IntSet.insert property mortgages
                                    money' = money + ((head $ board IntMap.! property) `div` 2)
                                in divest properties real_estate mortgages' money'

               -- The player has real estate to sell.  The player must have a monopoly of a color group if any property in the group
               -- has an improvement.  Houses/hotels must be sold from the most improved lot first (opposite of the way they are acquired).
               else let (property, _) = IntMap.findMin improved_real_estate  -- Divest from the lowest rent properties first.
                        color_group = colorSet property  -- Properties (IntSet)
                        (color_group_max_key, color_group_max_value) = IntMap.findMax $ IntMap.restrictKeys improved_real_estate color_group
                        real_estate' = IntMap.insert color_group_max_key (color_group_max_value - 1) real_estate
                        money' = money + ((housingCost color_group_max_key) `div` 2)
                    in divest properties real_estate' mortgages money'


getPurchasePrice :: Int -> Maybe Int
getPurchasePrice position = case (IntMap.lookup position board) of
                                Nothing -> Nothing
                                Just money_list -> Just (head money_list)


getRentDue :: RealEstate -> Int -> Int -> Int -> Players -> Int -> Int
-- Assume the property is not mortgaged.  This is tested upstream.
getRentDue real_estate current_player_index landlord_index position players dice_sum
    | current_player_index == landlord_index = 0  -- The current player owns the property.  No rent is due.
    | IntSet.member position utilities =
        -- Rent depends on the number of utilities the landlord owns and the roll of dice that landed the player on the utility.
        -- TODO - A player can land on a utility by drawing a chance card.  This occurrence, which is not implemented, would require rolling dice.
        let number_utilities_owned = IntSet.size $ IntSet.intersection utilities landlord_properties
        in dice_sum * (if number_utilities_owned == 1 then 4 else 10)
    | IntSet.member position railroads =
        -- Rent depends on the number of railroads the landlord owns.
        let number_railroads_owned = IntSet.size $ IntSet.intersection railroads landlord_properties
        in 2 ^ (number_railroads_owned - 1) * 25  -- $25, $50, $100, $200
    | otherwise =
        -- This is a "normal" property that might have houses or a hotel.
        case (IntMap.lookup position real_estate) of
            Nothing -> 0  -- The position is not ownable.
            Just development -> let money_list = board IntMap.! position
                                    multiplier = if development == 0 && hasMonopoly position landlord_properties
                                                     then 2  -- The property doesn't have any real estate, but the landlord has a monopoly.  This doubles the rent.
                                                     else 1
                                in (money_list !! (development + 1)) * multiplier  -- Pay this amount of rent.
    where (_, _, landlord_properties) = players !! landlord_index  -- TODO - The first guard doesn't use this.


hasMonopoly :: Int-> Properties -> Bool
-- Return True if the given properties include all of the properties associated with the given position.  (a.k.a. The landlord owns all properties of the color.)
-- Otherwise, return False.  Only call this function for properties that might have real estate.
hasMonopoly position landlord_properties
    | IntSet.member position purple = IntSet.isSubsetOf purple landlord_properties
    | IntSet.member position lightBlue = IntSet.isSubsetOf lightBlue landlord_properties
    | IntSet.member position pink = IntSet.isSubsetOf pink landlord_properties
    | IntSet.member position orange = IntSet.isSubsetOf orange landlord_properties
    | IntSet.member position red = IntSet.isSubsetOf red landlord_properties
    | IntSet.member position yellow = IntSet.isSubsetOf yellow landlord_properties
    | IntSet.member position green = IntSet.isSubsetOf green landlord_properties
    | IntSet.member position darkBlue = IntSet.isSubsetOf darkBlue landlord_properties


monopolizedColor :: Properties -> Maybe Properties
-- If the given properties includes a monopoly of any color group, return the monopolized set.
-- The order of preference is hardcoded going clockwise around the board from Go.
monopolizedColor properties
    | IntSet.null properties = Nothing
    | IntSet.isSubsetOf purple properties = Just purple
    | IntSet.isSubsetOf lightBlue properties = Just lightBlue
    | IntSet.isSubsetOf pink properties = Just pink
    | IntSet.isSubsetOf orange properties = Just orange
    | IntSet.isSubsetOf red properties = Just red
    | IntSet.isSubsetOf yellow properties = Just yellow
    | IntSet.isSubsetOf green properties = Just green
    | IntSet.isSubsetOf darkBlue properties = Just darkBlue
    | otherwise = Nothing


colorSet :: Int -> Properties
-- Given a property assumed to be in one of the color groups, return the set of all properties in the color group.
colorSet position
    | IntSet.member position purple = purple
    | IntSet.member position lightBlue = lightBlue
    | IntSet.member position pink = pink
    | IntSet.member position orange = orange
    | IntSet.member position red = red
    | IntSet.member position yellow = yellow
    | IntSet.member position green = green
    | IntSet.member position darkBlue = darkBlue


unmortgageProperties :: Properties -> Properties -> Int -> (Properties, Int)
-- Given a set of the properties owned by the current player, a global set of mortgaged proerties, and the current
-- player's money, spend money to unmortgage properties, if possible.  Return the new state of mortgages and money.
unmortgageProperties properties mortgages money =
    let mortgaged_properties = IntSet.intersection properties mortgages  -- Mortgaged properties owned by the current player
        (mortgaged_properties', money') = unmortgagePropertiesHelper (IntSet.toList mortgaged_properties) mortgages money
        newly_unmortgaged_properties = IntSet.difference mortgaged_properties mortgaged_properties'
        mortgages' = IntSet.difference mortgages newly_unmortgaged_properties
    in (mortgages', money')


unmortgagePropertiesHelper :: [Int] -> Properties -> Int -> (Properties, Int)
unmortgagePropertiesHelper [] mortgages money = (mortgages, money)
unmortgagePropertiesHelper (p:properties) mortgages money
    | money < unmortgage_price = (mortgages, money)  -- The prices become more expensive as one moves around the board, so recursing is not required.
    | otherwise =
        -- Unmortgage the property, and recurse.
        let mortgages' = IntSet.delete p mortgages
            money' = money - unmortgage_price
        in unmortgagePropertiesHelper properties mortgages' money'
    where unmortgage_price = round $ fromIntegral(head (board IntMap.! p)) * 0.55  -- Half the purchase price plus 10%


developeRealEstate :: RealEstate -> Properties -> Int -> (RealEstate, Int)
-- Given the state of the real estate, a subset of unmortgaged properties presumably owned by the same landlord (a monopoly),
-- and an amount of money (budget), develope the properties as much as possible.  See the rules for the constraints.
-- Return a 2-tuple of the modified real estate and the remaining budget.
developeRealEstate real_estate properties budget
    | budget < 50 = (real_estate, budget)  -- The player cannot afford the least expensive house.
    | otherwise = let housing_cost = housingCost $ IntSet.findMax properties
                  in developeRealEstateHelper real_estate properties budget housing_cost


developeRealEstateHelper :: RealEstate -> Properties -> Int -> Int -> (RealEstate, Int)
developeRealEstateHelper real_estate properties budget housing_cost
    -- Base case: The properties are fully developed, or the player cannot afford another house.
    | (minimum_development == 5) || (housing_cost > budget) = (real_estate, budget)
    | otherwise = let real_estate' = IntMap.insert least_developed_index (minimum_development + 1) real_estate
                      budget' = budget - housing_cost
                  in developeRealEstate real_estate' properties budget'
    -- Determine which property is least developed.
    -- TODO - restricted_properties doesn't need to be calculated every iteration.  Maybe make it a parameter.
    where (least_developed_index, minimum_development) = let restricted_properties = IntMap.restrictKeys real_estate properties
                                                             initial_accum = IntMap.findMin restricted_properties
                                                         in IntMap.foldlWithKey (\(a, b) k v -> if (v < b) then (k, v) else (a, b)) initial_accum restricted_properties


housingCost :: Int -> Int
-- Given the index of a property, return the cost of a new house or hotel for that property.
-- Note that real estate costs increase for each color group moving clockwise around the board.
housingCost property_index
    | property_index < 10 = 50
    | property_index < 20 = 100
    | property_index < 30 = 150
    | property_index < 40 = 200


tenPercent :: RealEstate -> Player -> Int
-- Return 10% (as an interger) of the player's total assets.
tenPercent real_estate (_, money, properties) =
    let land_equity = IntSet.foldl (\equity property -> equity + (head $ board IntMap.! property)) 0 properties  -- Aggregate purchase price of properties owned
        improveable_properties = [property | property <- IntSet.elems properties, IntMap.member property real_estate]
        real_estate_equity = foldl (\equity property -> equity + ((real_estate IntMap.! property) * (housingCost property))) 0 improveable_properties
    in round $ (fromIntegral (money + land_equity + real_estate_equity)) / 10

