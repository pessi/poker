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
import Data.Maybe

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Int } | Joker
    deriving (Show, Eq, Ord)

data Hand =
  HighCard
  | Pair
  | TwoPairs
  | ThreeOfKind
  | Straight
  | Flush
  | FullHouse
  | StraightFlush
  | FourOfKind
  | RoyalFlush
  | FiveOfKind
  deriving (Eq, Ord, Show)

checkHand :: [Card] -> Hand
checkHand cards = hand $ rankHand cards

data HandRank = HandRank { hand :: Hand, ranks :: [Int] }
     deriving (Show, Eq, Ord)

rankHand :: [Card] -> HandRank
rankHand cards
  | fiveOfKind = HandRank FiveOfKind best5
  | royalFlush = HandRank RoyalFlush [14]
  | straightFlush = HandRank StraightFlush sf5
  | fourOfKind = HandRank FourOfKind best5
  | fullHouse = HandRank FullHouse best5
  | flush = HandRank Flush flush5
  | straight = HandRank Straight str5
  | threeOfKind = HandRank ThreeOfKind best5
  | twoPairs = HandRank TwoPairs best5
  | pair = HandRank Pair best5
  | otherwise = HandRank HighCard best5
  where
    cs = [ c | c <- cards, c /= Joker ]
    joker = any (Joker ==) cards

    replace1by14 x = if x == 1 then 14 else x
    kindsOf rs = sortBy (flip compare) $ map replace1by14 rs
    longestFirst a b = compare (length b) (length a)
    kinds = sortBy longestFirst $ group $ kindsOf $ map rank cs
    nOfKind = length $ head kinds
    mOfKind = length $ if tail kinds /= []
                       then head $ tail kinds
                       else []
    best5 = if joker
            then take 5 $ (head $ head kinds) : concat kinds
            else take 5 $ concat kinds
    pair = nOfKind == 2
           || joker
    twoPairs = nOfKind == 2 && mOfKind == 2
    threeOfKind = nOfKind == 3
                  || joker && nOfKind == 2
    fullHouse = nOfKind == 3 && mOfKind >= 2
                || joker && nOfKind == 2 && mOfKind == 2
    fourOfKind = nOfKind == 4
                 || joker && nOfKind == 3
    fiveOfKind = nOfKind == 4 && joker

    wrapped = wrapAce $ map rank cs
    straight = hasStraight wrapped
               || joker && hasWildStraight wrapped
    str5 = fromJust $ fromJust $ find isJust $
           [ getStraight wrapped, getWildStraight wrapped ]

    suits = sortBy longestFirst $ group $ sort $ map suit cs
    longestSuit = head $ head suits
    nOfSuits = length $ head suits
    flush = nOfSuits == 5
            || joker && nOfSuits == 4

    flush5 = take 5 $ kindsOf [ rank c | c <- cs, suit c == longestSuit]

    wrappedBySuit = map wrapAce $ bySuit cs
    straightFlush = any hasStraight wrappedBySuit
                    || joker && any hasWildStraight wrappedBySuit

    sf5 = fromJust $ fromJust $ find isJust $
          map getStraight wrappedBySuit ++
          map getWildStraight wrappedBySuit

    hasRoyalStraight xs = hasStraight [x | x <- xs, x >= 10]
    hasWildRoyalStraight xs = hasWildStraight [x | x <- xs, x >= 10]
    royalFlush = any hasRoyalStraight wrappedBySuit
                 || joker && any hasWildRoyalStraight wrappedBySuit

hasStraight xs = isJust $ getStraight xs
getStraight = getNStraight 5

getNStraight :: Int -> [Int] -> Maybe [Int]
getNStraight n xs
  | length xs < n = Nothing
  | nx == take n xs = Just nx
  | otherwise = getNStraight n $ tail xs
  where x = head xs
        nx = [x,x-1 .. x - n + 1]

wrapAce :: [Int] -> [Int]
wrapAce rs
  | null rs = []
  | last nubs == 1 = 14 : nubs
  | otherwise = nubs
  where nubs = nub $ sortBy (flip compare) rs

hasWildStraight :: [Int] -> Bool
hasWildStraight ranks = isJust $ getWildStraight ranks

getWildStraight :: [Int] -> Maybe [Int]
getWildStraight ranks
   | length ranks < 4 = Nothing -- Optimization. Whee
   | isNothing result = Nothing
   | otherwise = fromJust result
  where
    rs = wrapAce ranks
    rss = [ insert r rs | r <- [14,13..1], r `notElem` rs]
    result = find isJust $ map getStraight rss

bySuit cards = [[ rank c | c <- cards, suit c == s]
               | s <- [Hearts, Spades, Diamonds, Clubs]]
