{-

Homework assignment for Haskell Dojo #1
---------------------------------------

Implement a function that takes as input a list of cards and returns what poker hand it was.

You can see below for data type declarations that you can use for representing the input and output of this function.

Feel free to modify them any way you want.

Implementing support for the "Joker" (or wildcard?) card is optional.  The reference implementation (to be posted later) does not implement joker handling because the solution is easier to read without them.

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
