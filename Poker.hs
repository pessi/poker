{-

Homework assignment for Haskell Dojo #1
---------------------------------------

1) Implement a function that takes as input a list of cards and
returns what poker hand it was.

You can see below for data type declarations that you can use for
representing the input and output of this function.

Feel free to modify them any way you want.  I highly recommend you to
look into what functions there are in Prelude and in Data.List and use
library functions as appropriate.

Implementing support for the "Joker" (or wildcard?) card is optional.
The reference implementation (to be posted later) does not implement
joker handling because the solution is easier to read without them.

Use this as the spec for poker hands:

http://ourpokerleague.co.uk/img/poker_hands.gif

The topmost is the most valuable hand, so match that first.  E.g.,
a royal flush of [A14, A13, A12, A11, A10] is also a straight and a
flush.  But your function should detect that case as the royal flush.

Examples:

*Cards> checkHand [Card Diamonds 3, Card Hearts 3, Card Diamonds 5, Card Spades 6, Card Spades 7]
Pair

*Cards> checkHand [Card Diamonds 3, Card Hearts 4, Card Diamonds 5, Card Spades 6, Card Spades 7]
Straight

I have unit tests for my own solutions, so if you stick to the
reference data types, we might be able to run your solutions against
my tests. :)


Optional 2) Implement Joker handling.


Optional 3) Implement hand comparison.

E.g., write a function that compares two
hands and determines which scores higher.  For example, a pair of 10's
scores higher than a pair of 2's.

-}

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Int } | Joker
    deriving (Show, Eq, Ord)

data Hand =
    RoyalFlush
  | StraightFlush
  | FourOfKind
  | FullHouse
  | Flush
  | Straight
  | ThreeOfKind
  | TwoPairs
  | Pair
  | HighCard
  deriving (Eq, Show)


checkHand :: [Card] -> Hand
-- TODO implement this
