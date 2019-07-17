# monopoly-monte-carlo

How do variations in starting conditions affect players' chances of winning?

Developed and tested with GHC 8.6.5.

## Branches
* master - All players start with equal assets
* double_money_first - The first player starts with twice as much money as the other players
* double_money_last - The last player starts with twice as much money as the other players
* last_baltic - The last player starts with Baltic Avenue
* last_boardwalk - The last player starts with Boardwalk

## Strategy and Rule Modifications
All players follow a simple, hard-coded strategy.  An open question, requiring an AI approach to answer,
is if a player following an optimal strategy can overcome an initial disadvantage against suboptimal
adversaries.

With the [official rules](https://en.wikibooks.org/wiki/Monopoly/Official_Rules) in mind:

1. There are no interplayer transactions other than rent payments and bulk transfers upon bankruptcy.
2. An unowned property is not auctioned when a player lands on it and declines to buy.
3. A jail player always moves to "Just Visiting" on its next turn.  It does not try to roll dice for freedom.
4. When a player lands on an unowned property, it always buys the property if it has adequate funds.
5. All real estate development happens in a clockwise order of preference.  For example, if the player
has a monopoly on Mediterranean-Baltic and a monopoly on Park Place-Boardwalk, the player will add
improvements to the former first as long as room for improvements remains.
6. Divesting real estate, to pay rent or taxes, also follows a clockwise precedence.
7. "Chance" and "Community Chest" are not implemented.  These spaces on the board are ignored.

## Turns Limit
Without the posibility of players making deals, [Monopoly games can last indefinitely](https://www.informs-sim.org/wsc09papers/036.pdf).
To mitigate this infinite loop condition, a game is abandoned when there is no winner after each player has taken 20,000 turns.  The
simulation continues until completing the user-specified number of games.
