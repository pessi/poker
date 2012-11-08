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

module Poker where

import Data.List
import Data.Eq

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Int } | Joker
    deriving (Show, Eq, Ord)

data Hand =
  FiveOfKind
  | RoyalFlush
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
checkHand cards
  | fiveOfKind = FiveOfKind
  | royalFlush = RoyalFlush
  | straightFlush = StraightFlush
  | fourOfKind = FourOfKind
  | fullHouse = FullHouse
  | flush = Flush
  | straight = Straight
  | threeOfKind = ThreeOfKind
  | twoPairs = TwoPairs
  | pair = Pair
  | otherwise = HighCard
  where
    cs = [ c | c <- cards, c /= Joker ]
    joker = any (Joker ==) cards
    ranks = sort $ map rank cs
    suits = sort $ map suit cs
    revByLength a b = compare (length b) (length a)
    groupedKind = sortBy revByLength $ group ranks
    nOfKind = length $ head groupedKind
    mOfKind = if tail groupedKind /= [] then length $ groupedKind !! 1 else 0
    pair = nOfKind == 2
           || joker
    twoPairs = nOfKind == 2 && mOfKind == 2
    threeOfKind = nOfKind == 3
                  || joker && nOfKind == 2
    fullHouse = nOfKind == 3 && mOfKind == 2
                || joker && nOfKind == 2 && mOfKind == 2
    fourOfKind = nOfKind == 4
                 || joker && nOfKind == 3
    fiveOfKind = nOfKind == 4 && joker
    straighted = straighten ranks
    straight = hasStraight straighted
               || joker && hasWildStraight straighted
    groupedSuit = sortBy revByLength $ group suits
    nOfSuits = length $ head groupedSuit
    flush = nOfSuits == 5
            || joker && nOfSuits == 4
    straightedBySuit = straightenBySuit cs
    straightFlush = any hasStraight straightedBySuit
                    || joker && any hasWildStraight straightedBySuit
    hasRoyalStraight xs = hasStraight [x | x <- xs, x >= 10]
    royalFlush = any hasRoyalStraight straightedBySuit
                 || joker && any hasWildRoyalStraight straightedBySuit

hasStraight = hasNStraight 5

hasNStraight :: Int -> [Int] -> Bool
hasNStraight n xs
  | length xs < n = False
  | otherwise = [x .. x + n - 1] == take n xs
  where x = head xs

straighten rs
  | null rs = []
  | head rs == 1 = nubs ++ [14]
  | otherwise = nubs
  where nubs = nub rs

hasWildStraight :: [Int] -> Bool
hasWildStraight ranks
  | length ranks < 4 = False
  | otherwise = any hasStraight rss
  where
    rs = straighten $ sort ranks
    rss = [ sort (r : rs) | r <- [1..14], not (r `elem` rs)]

straightenBySuit cards = [straighten $ sort [ rank c | c <- cards, suit c == s]
                       | s <- [Hearts, Spades, Diamonds, Clubs]]

hasRoyalStraight xs = hasStraight [x | x <- xs, x >= 10]
hasWildRoyalStraight xs = hasWildStraight [x | x <- xs, x >= 10]
