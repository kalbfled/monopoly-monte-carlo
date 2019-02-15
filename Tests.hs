-- (C) David J. Kalbfleisch 2019

-- https://hackage.haskell.org/package/HUnit
-- https://hoogle.haskell.org/?hoogle=test%20package:HUnit

import Monopoly
import MonopolyInit (initRealEstate)
import Test.HUnit
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List


testWinnerIndex1 = let players = [(1, 0, IntSet.empty), (1, 1, IntSet.empty)]
                   in TestCase (assertEqual "" Nothing (winnerIndex players))

testWinnerIndex2 = let players = [(1, 0, IntSet.empty), (1, -1, IntSet.empty)]
                   in TestCase (assertEqual "" (Just 0) (winnerIndex players))

testWinnerIndex3 = let players = [(1, -40, IntSet.empty), (1, 1, IntSet.empty)]
                   in TestCase (assertEqual "" (Just 1) (winnerIndex players))

testWinnerIndex = TestLabel "testWinnerIndex" $ TestList [testWinnerIndex1, testWinnerIndex2, testWinnerIndex3]


-- Nothing to divest.
testDivest1 = let properties = IntSet.empty
                  real_estate = IntMap.empty
                  mortgages = IntSet.empty
                  expected = (real_estate, mortgages, -20)
              in TestCase (assertEqual "" expected (divest properties real_estate mortgages (-20 :: Int)))

-- Sell one house, for $25, from one monopolized group.  No mortgages.
testDivest2 = let properties = IntSet.fromList [1, 3, 11, 13, 14]
                  real_estate = IntMap.fromList [(1, 1), (3, 2), (11, 1), (13, 0), (14, 0)]
                  real_estate' = IntMap.fromList [(1, 1), (3, 1), (11, 1), (13, 0), (14, 0)]
                  mortgages = IntSet.empty
                  expected = (real_estate', mortgages, 5)
              in TestCase (assertEqual "" expected (divest properties real_estate mortgages (-20 :: Int)))

-- Sell three houses, for $25 each, from one monopolized group.  No mortgages.
testDivest3 = let properties = IntSet.fromList [1, 3, 11, 13, 14]
                  real_estate = IntMap.fromList [(1, 1), (3, 2), (11, 1), (13, 0), (14, 0)]
                  real_estate' = IntMap.fromList [(1, 0), (3, 0), (11, 1), (13, 0), (14, 0)]
                  mortgages = IntSet.empty
                  expected = (real_estate', mortgages, 7)
              in TestCase (assertEqual "" expected (divest properties real_estate mortgages (-68 :: Int)))

-- Sell four houses from two monopolized groups.  No mortgages.
testDivest4 = let properties = IntSet.fromList [1, 3, 11, 13, 14]
                  real_estate = IntMap.fromList [(1, 1), (3, 2), (11, 1), (13, 0), (14, 0)]
                  real_estate' = IntMap.fromList [(1, 0), (3, 0), (11, 0), (13, 0), (14, 0)]
                  mortgages = IntSet.empty
                  expected = (real_estate', mortgages, 25)
              in TestCase (assertEqual "" expected (divest properties real_estate mortgages (-100 :: Int)))

-- Sell four houses from two monopolized groups, and mortgage one property.
testDivest5 = let properties = IntSet.fromList [1, 3, 11, 13, 14]
                  real_estate = IntMap.fromList [(1, 1), (3, 2), (11, 1), (13, 0), (14, 0)]
                  real_estate' = IntMap.fromList [(1, 0), (3, 0), (11, 0), (13, 0), (14, 0)]
                  mortgages = IntSet.empty
                  mortgages' = IntSet.singleton 1
                  expected = (real_estate', mortgages', 29)
              in TestCase (assertEqual "" expected (divest properties real_estate mortgages (-126 :: Int)))

-- Sell four houses from two monopolized groups, mortgage five properties, and remain insolvent.
testDivest6 = let properties = IntSet.fromList [1, 3, 11, 13, 14]
                  real_estate = IntMap.fromList [(1, 1), (3, 2), (11, 1), (13, 0), (14, 0)]
                  real_estate' = IntMap.fromList [(1, 0), (3, 0), (11, 0), (13, 0), (14, 0)]
                  mortgages = IntSet.empty
                  mortgages' = properties  -- All properties become mortgaged.
                  expected = (real_estate', mortgages', -1)
              in TestCase (assertEqual "" expected (divest properties real_estate mortgages (-416 :: Int)))

testDivest = TestLabel "testDivest" $ TestList [testDivest1, testDivest2, testDivest3, testDivest4, testDivest5, testDivest6]


testGetPurchasePrice1 = TestCase (assertEqual "" Nothing (getPurchasePrice 0))

testGetPurchasePrice2 = TestCase (assertEqual "" (Just 200) (getPurchasePrice 5))

testGetPurchasePrice3 = TestCase (assertEqual "" (Just 220) (getPurchasePrice 21))

testGetPurchasePrice = TestLabel "testGetPurchasePrice" $ TestList [testGetPurchasePrice1, testGetPurchasePrice2, testGetPurchasePrice3]


testLandlordLambda1 = let players = [(1, 0, IntSet.singleton 1), (1, 1, IntSet.empty)]
                          landlord = List.findIndex (\(_, _, properties) -> IntSet.member 1 properties) players  -- Maybe Int
                      in TestCase (assertEqual "" (Just 0) landlord)

testLandlordLambda2 = let players = [(1, 0, IntSet.singleton 1), (1, 1, IntSet.empty)]
                          landlord = List.findIndex (\(_, _, properties) -> IntSet.member 2 properties) players  -- Maybe Int
                      in TestCase (assertEqual "" Nothing landlord)

-- The function takeTurn contains a lambda function to determine the landlord for a given property.
testLandlordLambda = TestLabel "testLandlordLambda" $ TestList [testLandlordLambda1, testLandlordLambda2]


-- The player lands on its own property.  No rent is due.
testGetRentDue1 = let players = [(1, 0, IntSet.singleton 1), (1, 1, IntSet.empty)]
                      real_estate = IntMap.fromList [(1, 0), (3, 0)]
                  in TestCase (assertEqual "" 0 (getRentDue real_estate 0 0 1 players 2))

-- The player lands on another player's unimproved property, which is not part of a monopoly.  Pay rent.
testGetRentDue2 = let players = [(1, 0, IntSet.singleton 1), (1, 1, IntSet.empty)]
                      real_estate = IntMap.fromList [(1, 0), (3, 0)]
                  in TestCase (assertEqual "" 2 (getRentDue real_estate 1 0 1 players 2))

-- The player lands on another player's unimproved property, which is part of a monopoly.  Pay double rent.
testGetRentDue3 = let players = [(1, 0, IntSet.fromList [1, 3]), (1, 1, IntSet.empty)]
                      real_estate = IntMap.fromList [(1, 0), (3, 0)]
                  in TestCase (assertEqual "" 4 (getRentDue real_estate 1 0 1 players 2))

-- The player lands on another player's monopolized property that includes one house.  Pay rent.
testGetRentDue4 = let players = [(1, 0, IntSet.fromList [1, 3]), (1, 1, IntSet.empty)]
                      real_estate = IntMap.fromList [(1, 1), (3, 0)]
                  in TestCase (assertEqual "" 10 (getRentDue real_estate 1 0 1 players 2))

-- The player lands on another player's monopolized property that includes one hotel.  Pay rent.
testGetRentDue5 = let players = [(1, 0, IntSet.fromList [37, 39]), (39, 1, IntSet.empty)]
                      real_estate = IntMap.fromList [(37, 4), (39, 5)]
                  in TestCase (assertEqual "" 2000 (getRentDue real_estate 1 0 39 players 2))

-- The player lands on another player's utility, and the other player owns only one utility.  Pay rent.
testGetRentDue6 = let players = [(1, 0, IntSet.fromList [12]), (1, 1, IntSet.empty)]
                      real_estate = IntMap.empty
                  in TestCase (assertEqual "" 8 (getRentDue real_estate 1 0 12 players 2))

-- The player lands on another player's utility, and the other player owns both utilities.  Pay rent.
testGetRentDue7 = let players = [(1, 0, IntSet.fromList [12, 28]), (1, 1, IntSet.empty)]
                      real_estate = IntMap.empty
                  in TestCase (assertEqual "" 50 (getRentDue real_estate 1 0 12 players 5))

-- The player lands on another player's railroad, and the other player owns one railroad.  Pay rent.
testGetRentDue8 = let players = [(1, 0, IntSet.fromList [5]), (1, 1, IntSet.empty)]
                      real_estate = IntMap.empty
                  in TestCase (assertEqual "" 25 (getRentDue real_estate 1 0 5 players 5))

-- The player lands on another player's railroad, and the other player owns four railroads.  Pay rent.
testGetRentDue9 = let players = [(1, 0, IntSet.fromList [5, 15, 25, 35]), (1, 1, IntSet.empty)]
                      real_estate = IntMap.empty
                  in TestCase (assertEqual "" 200 (getRentDue real_estate 1 0 25 players 5))

-- Note that these tests all assume that the property for which rent is being calculated is unmortgaged.
-- This condition is checked upstream of the function under test.
testGetRentDue = TestLabel "testGetRentDue" $ TestList [testGetRentDue1, testGetRentDue2, testGetRentDue3, testGetRentDue4, testGetRentDue5,
                                                        testGetRentDue6, testGetRentDue7, testGetRentDue8, testGetRentDue9]


-- Inadequate money to unmortgage one property.
testUnmortgageProperties1 = let properties = IntSet.fromList [1, 9]
                                mortgages = IntSet.fromList [1, 39]
                                (mortgages', money') = unmortgageProperties properties mortgages 1
                            in TestCase (assertEqual "" (mortgages, 1) (mortgages', money'))

-- Unmortgage one property.
testUnmortgageProperties2 = let properties = IntSet.fromList [1, 9]
                                mortgages = IntSet.fromList [1, 39]
                                (mortgages', money') = unmortgageProperties properties mortgages 263
                            in TestCase (assertEqual "" (IntSet.singleton 39, 230) (mortgages', money'))

-- Unmortgage two properties.
testUnmortgageProperties3 = let properties = IntSet.fromList [1, 9, 39]
                                mortgages = IntSet.fromList [1, 39]
                                (mortgages', money') = unmortgageProperties properties mortgages 263
                            in TestCase (assertEqual "" (IntSet.empty, 10) (mortgages', money'))

testUnmortgageProperties = TestLabel "testUnmortgageProperties" $ TestList [testUnmortgageProperties1, testUnmortgageProperties2, testUnmortgageProperties3]


-- Inadequate money to buy a house.
testDevelopeRealEstate1 = let real_estate = IntMap.fromList [(1, 1), (3, 0)]
                              properties = IntSet.fromList [1, 3]
                          in TestCase (assertEqual "" (real_estate, 49) (developeRealEstate real_estate properties 49))

-- Build one house on one property when no property is improved.
testDevelopeRealEstate2 = let real_estate = IntMap.fromList [(1, 0), (3, 0)]
                              properties = IntSet.fromList [1, 3]
                              real_estate' = IntMap.fromList [(1, 1), (3, 0)]
                          in TestCase (assertEqual "" (real_estate', 1) (developeRealEstate real_estate properties 51))

-- Build one house on one property when one property is already improved.
testDevelopeRealEstate3 = let real_estate = IntMap.fromList [(1, 1), (3, 0)]
                              properties = IntSet.fromList [1, 3]
                              real_estate' = IntMap.fromList [(1, 1), (3, 1)]
                          in TestCase (assertEqual "" (real_estate', 1) (developeRealEstate real_estate properties 51))

-- Starting with two unimproved lots, build one hotel on one property and four houses on the other property.
testDevelopeRealEstate4 = let real_estate = IntMap.fromList [(1, 0), (3, 0)]
                              properties = IntSet.fromList [1, 3]
                              real_estate' = IntMap.fromList [(1, 5), (3, 4)]
                          in TestCase (assertEqual "" (real_estate', 1) (developeRealEstate real_estate properties 451))

-- The lots are maxed out and cannot be improved further.
testDevelopeRealEstate5 = let real_estate = IntMap.fromList [(37, 5), (39, 5)]
                              properties = IntSet.fromList [37, 39]
                          in TestCase (assertEqual "" (real_estate, 50000) (developeRealEstate real_estate properties 50000))

-- Note that these tests all assume that the properties to develope are unmortgaged and part of the same color group.
-- This condition is checked upstream of the function under test.
testDevelopeRealEstate = TestLabel "testDevelopeRealEstate" $ TestList [testDevelopeRealEstate1, testDevelopeRealEstate2, testDevelopeRealEstate3,
                                                                        testDevelopeRealEstate4, testDevelopeRealEstate5]


-- The player has nothing.
testTenPercent1 = let real_estate = IntMap.empty
                      player = (4, 0, IntSet.empty)
                  in TestCase (assertEqual "" 0 (tenPercent real_estate player))

-- The player only has cash.
testTenPercent2 = let real_estate = IntMap.empty
                      player = (4, 10, IntSet.empty)
                  in TestCase (assertEqual "" 1 (tenPercent real_estate player))

-- The player only has unimproved property.
testTenPercent3 = let real_estate = IntMap.empty
                      player = (4, 0, IntSet.fromList [1, 39])
                  in TestCase (assertEqual "" 46 (tenPercent real_estate player))

-- The player has improved and unimproved property.
testTenPercent4 = let real_estate = IntMap.fromList [(1, 0), (39, 2)]
                      player = (4, 0, IntSet.fromList [1, 39])
                  in TestCase (assertEqual "" 86 (tenPercent real_estate player))

-- The player has cash, improved property, and unimproved property.
testTenPercent5 = let real_estate = IntMap.fromList [(1, 0), (39, 2)]
                      player = (4, 100, IntSet.fromList [1, 39])
                  in TestCase (assertEqual "" 96 (tenPercent real_estate player))

testTenPercent = TestLabel "testTenPercent" $ TestList [testTenPercent1, testTenPercent2, testTenPercent3, testTenPercent4, testTenPercent5]


-- On an incarcerated player's turn, move it to "Just Visiting".
testTakeTurn1 = let players = [(jail, 200, IntSet.empty), (1, 200, IntSet.empty)]
                    real_estate = initRealEstate
                    mortgages = IntSet.empty
                    dice = [1..5]
                    players' = [(justVisiting, 200, IntSet.empty), (1, 200, IntSet.empty)]
                    result = takeTurn players real_estate mortgages 0 0 dice
                in TestLabel "Get out of jail" $ TestCase (assertEqual "" (players', real_estate, mortgages, dice) result)

-- The player rolls doubles once.  Take two turns.
testTakeTurn2 = let players = replicate 2 (0, 1500, IntSet.empty)
                    real_estate = initRealEstate
                    mortgages = IntSet.empty
                    dice = [3, 3, 1, 2, 3]
                    players' = [(9, 1280, IntSet.fromList [6, 9]), (0, 1500, IntSet.empty)]
                    result = takeTurn players real_estate mortgages 0 0 dice
                in TestLabel "Rolled doubles x1" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player rolls doubles again after already having rolled doubles twice.  Go to jail.
testTakeTurn3 = let players = replicate 2 (0, 1500, IntSet.empty)
                    real_estate = initRealEstate
                    mortgages = IntSet.empty
                    dice = [3, 3, 1, 2, 3]
                    players' = [(jail, 1500, IntSet.empty), (0, 1500, IntSet.empty)]
                    result = takeTurn players real_estate mortgages 0 2 dice
                in TestLabel "Triple Doubles" $ TestCase (assertEqual "" (players', real_estate, mortgages, [1..3]) result)

-- The player lands on "Go to jail".
testTakeTurn4 = let players = replicate 2 (27, 1500, IntSet.empty)
                    real_estate = initRealEstate
                    mortgages = IntSet.empty
                    dice = [1, 2, 3]
                    players' = [(jail, 1500, IntSet.empty), (27, 1500, IntSet.empty)]
                    result = takeTurn players real_estate mortgages 0 1 dice
                in TestLabel "Go to jail" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on "Income Tax".  Pay 10%.
testTakeTurn5 = let players = replicate 2 (0, 1500, IntSet.empty)
                    real_estate = initRealEstate
                    mortgages = IntSet.empty
                    dice = [1, 3, 3]
                    players' = [(incomeTax, 1350, IntSet.empty), (0, 1500, IntSet.empty)]
                    result = takeTurn players real_estate mortgages 0 0 dice
                in TestLabel "Income Tax 10%" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on "Income Tax".  Pay $200.
testTakeTurn6 = let players = replicate 2 (0, 3000, IntSet.empty)
                    real_estate = initRealEstate
                    mortgages = IntSet.empty
                    dice = [1, 3, 3]
                    players' = [(incomeTax, 2800, IntSet.empty), (0, 3000, IntSet.empty)]
                    result = takeTurn players real_estate mortgages 0 0 dice
                in TestLabel "Income Tax $200" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on "Free Parking".  No change, other than the position.
testTakeTurn7 = let players = replicate 2 (16, 1500, IntSet.empty)
                    real_estate = initRealEstate
                    mortgages = IntSet.empty
                    dice = [1, 3, 3]
                    players' = [(20, 1500, IntSet.empty), (16, 1500, IntSet.empty)]
                    result = takeTurn players real_estate mortgages 0 0 dice
                in TestLabel "Free Parking" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on "Go".  Collect $200.
testTakeTurn8 = let players = replicate 2 (36, 1500, IntSet.empty)
                    real_estate = initRealEstate
                    mortgages = IntSet.empty
                    dice = [1, 3, 3]
                    players' = [(0, 1700, IntSet.empty), (36, 1500, IntSet.empty)]
                    result = takeTurn players real_estate mortgages 0 0 dice
                in TestLabel "Free Parking" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on "Luxury Tax" and has enough cash.  Pay $100.
testTakeTurn9 = let players = replicate 2 (34, 1500, IntSet.empty)
                    real_estate = initRealEstate
                    mortgages = IntSet.empty
                    dice = [1, 3, 3]
                    players' = [(luxuryTax, 1400, IntSet.empty), (34, 1500, IntSet.empty)]
                    result = takeTurn players real_estate mortgages 0 0 dice
                in TestLabel "Luxury Tax $100" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player is bankrupt.  Do not change the game state.
testTakeTurn10 = let players = [(luxuryTax, -50, IntSet.empty), (34, 50, IntSet.empty)]
                     real_estate = initRealEstate
                     mortgages = IntSet.empty
                     dice = [1, 3, 3]
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Bankrupt" $ TestCase (assertEqual "" (players, real_estate, mortgages, dice) result)

-- The player lands on "Luxury Tax", does not have enough cash, and does not have any equity to divest.  Go bankrupt.
testTakeTurn11 = let players = replicate 2 (34, 50, IntSet.empty)
                     real_estate = initRealEstate
                     mortgages = IntSet.empty
                     dice = [1, 3, 3]
                     players' = [(luxuryTax, -50, IntSet.empty), (34, 50, IntSet.empty)]
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Luxury Tax Bankrupt 1" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on "Luxury Tax", does not have enough cash, and must sell a house to remain solvent.
testTakeTurn12 = let players = [(34, 76, IntSet.fromList [1, 3]), (34, 76, IntSet.empty)]
                     real_estate = IntMap.insert 1 1 initRealEstate
                     mortgages = IntSet.empty
                     dice = [1, 3, 3]
                     players' = [(luxuryTax, 1, IntSet.fromList [1, 3]), (34, 76, IntSet.empty)]
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Luxury Tax Divest 1" $ TestCase (assertEqual "" (players', initRealEstate, mortgages, [3]) result)

-- The player lands on "Luxury Tax", and does not have enough cash.  The player must sell a house and mortgage a property to remain solvent.
testTakeTurn13 = let players = [(34, 46, IntSet.fromList [1, 3]), (34, 76, IntSet.empty)]
                     real_estate = IntMap.insert 1 1 initRealEstate
                     mortgages = IntSet.empty
                     dice = [1, 3, 3]
                     players' = [(luxuryTax, 1, IntSet.fromList [1, 3]), (34, 76, IntSet.empty)]
                     mortgages' = IntSet.singleton 1
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Luxury Tax Divest 2" $ TestCase (assertEqual "" (players', initRealEstate, mortgages', [3]) result)

-- The player lands on "Luxury Tax", and does not have enough cash.  The player must sell a house and mortgage two properties to remain solvent.
testTakeTurn14 = let players = [(34, 6, IntSet.fromList [1, 3]), (34, 76, IntSet.empty)]
                     real_estate = IntMap.insert 1 1 initRealEstate
                     mortgages = IntSet.empty
                     dice = [1, 3, 3]
                     players' = [(luxuryTax, 1, IntSet.fromList [1, 3]), (34, 76, IntSet.empty)]
                     mortgages' = IntSet.fromList [1, 3]
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Luxury Tax Divest 3" $ TestCase (assertEqual "" (players', initRealEstate, mortgages', [3]) result)

-- The player lands on "Luxury Tax", and does not have enough cash.  The player sells a house and mortgages two properties
-- but still goes bankrupt.  The player's properties revert to the bank a become unmortgaged.
testTakeTurn15 = let players = [(34, 4, IntSet.fromList [1, 3]), (34, 76, IntSet.empty)]
                     real_estate = IntMap.insert 1 1 initRealEstate
                     mortgages = IntSet.empty
                     dice = [1, 3, 3]
                     players' = [(luxuryTax, -1, IntSet.empty), (34, 76, IntSet.empty)]
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Luxury Tax Bankrupt 2" $ TestCase (assertEqual "" (players', initRealEstate, mortgages, [3]) result)

-- The player lands on another player's unimproved, unmonopolized, unmortgaged property.  Pay rent.
testTakeTurn16 = let players = [(0, 1500, IntSet.empty), (0, 1500, IntSet.singleton 3)]
                     real_estate = initRealEstate
                     mortgages = IntSet.empty
                     dice = [1..3]
                     players' = [(3, 1496, IntSet.empty), (0, 1504, IntSet.singleton 3)]
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Pay Rent 1" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on another player's unimproved, monopolized, unmortgaged property.  Pay double rent.
testTakeTurn17 = let players = [(0, 1500, IntSet.empty), (0, 1500, IntSet.fromList [1, 3])]
                     real_estate = initRealEstate
                     mortgages = IntSet.empty
                     dice = [1..3]
                     players' = [(3, 1492, IntSet.empty), (0, 1508, IntSet.fromList [1, 3])]
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Pay Rent 2" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on another player's improved, unmortgaged property.  Pay rent.
testTakeTurn18 = let players = [(0, 1500, IntSet.empty), (0, 1500, IntSet.fromList [1, 3])]
                     real_estate = IntMap.insert 3 1 initRealEstate
                     mortgages = IntSet.empty
                     dice = [1..3]
                     players' = [(3, 1480, IntSet.empty), (0, 1520, IntSet.fromList [1, 3])]
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Pay Rent 3" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on another player's mortgaged property.  No rent is due.
testTakeTurn19 = let players = [(0, 1500, IntSet.empty), (0, 1500, IntSet.fromList [1, 3])]
                     real_estate = initRealEstate
                     mortgages = IntSet.singleton 3
                     dice = [1..3]
                     players' = [(3, 1500, IntSet.empty), (0, 1500, IntSet.fromList [1, 3])]
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Pay Rent 4" $ TestCase (assertEqual "" (players', real_estate, mortgages, [3]) result)

-- The player lands on another player's unmproved, unmortgaged property.  The player must divest to pay rent.
testTakeTurn20 = let players = [(0, 0, IntSet.singleton 9), (0, 1500, IntSet.fromList [1, 3])]
                     real_estate = initRealEstate
                     mortgages = IntSet.empty
                     dice = [1..3]
                     players' = [(3, 52, IntSet.singleton 9), (0, 1508, IntSet.fromList [1, 3])]
                     mortgages' = IntSet.singleton 9
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Pay Rent 5" $ TestCase (assertEqual "" (players', real_estate, mortgages', [3]) result)

-- The player lands on another player's improved, unmortgaged property.  The player divests to pay rent
-- but still goes bankrupt.  The player's properties revert to the landlord and remain mortgaged.
testTakeTurn21 = let players = [(0, 0, IntSet.singleton 9), (0, 1500, IntSet.fromList [1, 3])]
                     real_estate = IntMap.insert 3 3 initRealEstate
                     mortgages = IntSet.empty
                     dice = [1..3]
                     players' = [(3, -120, IntSet.empty), (0, 1500, IntSet.fromList [1, 3, 9])]
                     mortgages' = IntSet.singleton 9
                     result = takeTurn players real_estate mortgages 0 0 dice
                 in TestLabel "Pay Rent Bankrupt" $ TestCase (assertEqual "" (players', real_estate, mortgages', [3]) result)

-- The player passes go and lands on a property that completes a monopoly.  The player buys it and improves the monopolized properties.
testTakeTurn22 = let players = [(0, 1500, IntSet.empty), (38, 1500, IntSet.singleton 3)]
                     real_estate = initRealEstate
                     mortgages = IntSet.empty
                     dice = [1..3]
                     players' = [(0, 1500, IntSet.empty), (1, 1140, IntSet.fromList [1, 3])]
                     real_estate' = IntMap.insert 3 5 $ IntMap.insert 1 5 initRealEstate
                     result = takeTurn players real_estate mortgages 1 0 dice
                 in TestLabel "Invest" $ TestCase (assertEqual "" (players', real_estate', mortgages, [3]) result)

testTakeTurn = TestLabel "testTakeTurn" $ TestList [testTakeTurn1, testTakeTurn2, testTakeTurn3, testTakeTurn4, testTakeTurn5, testTakeTurn6, testTakeTurn7, testTakeTurn8,
                                                    testTakeTurn9, testTakeTurn10, testTakeTurn11, testTakeTurn12, testTakeTurn13, testTakeTurn14, testTakeTurn15, testTakeTurn16,
                                                    testTakeTurn17, testTakeTurn18, testTakeTurn19, testTakeTurn20, testTakeTurn21, testTakeTurn22]


main = do
    runTestTT testWinnerIndex
    runTestTT testDivest
    runTestTT testGetPurchasePrice
    runTestTT testLandlordLambda
    runTestTT testGetRentDue
    runTestTT testUnmortgageProperties
    runTestTT testDevelopeRealEstate
    runTestTT testTenPercent
    runTestTT testTakeTurn

