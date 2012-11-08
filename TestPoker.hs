import Poker
import Data.List

h a = Card Hearts a
s a = Card Spades a
d a = Card Diamonds a
c a = Card Clubs a
j = Joker

h1 = [ h 1, h 3, s 4, d 6, c 9 ]
h2 = [ h 1, h 3, s 4, d 6, c 4 ]
h3 = [ h 1, d 4, s 4, d 6, c 4 ]
h32 = [ h 1, d 4, s 4, d 1, c 4 ]
h4 = [ h 4, d 4, s 4, d 1, c 4 ]
hs = [ h 1, d 5, s 4, c 2, d 3 ]
hf = [ h 1, h 9, h 4, h 2, h 5 ]
hsf = ( h 3 ) : hf
hrf = [ h 1, h 10, h 13, h 12, h 11 ]

hands = [h1, h2, h3, h32, h4, hs, hf, hsf, hrf ]

jhands = map (\ h -> j : h) hands

results = map checkHand hands
jresults = map checkHand jhands

perms [] = [[]]
perms xs = [ x:ps | x <- xs , ps <- perms ( xs \\ [x] ) ]

-- subsets of size k
combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs)
  | k > length xs + 1 = []
  | otherwise = map (x:) (combs (k-1) xs) ++ combs k xs

suits = [Hearts, Spades, Diamonds, Clubs]

handify :: [Int] -> [Card]
handify rs = map (\ (s, r) -> Card s r) $ zip (cycle suits) rs

heartify :: [Int] -> [Card]
heartify rs = map (Card Hearts) rs

modRank :: Int -> Int
modRank n
  | n < 1 = n + 13
  | n > 13 = n - 13
  | otherwise = n

l2h :: [Int]
l2h = [2..13] ++ [1]

{- there will be 7,462 distinct hands in 5-card poker -}
ranking5 :: [[Card]]
ranking5 = highCards ++
       pairs ++
       twoPairs ++
       threeOfKinds ++
       straights ++
       flushes ++
       fullHouses ++
       straightFlushes ++
       fourOfKinds ++
       royalFlush
  where
    highs :: [[Int]]
    highs = concat ([ map (n :) (init $ combs 4 [2..n-1]) | n <- [7..13]] ++
                     [ map (1 :) (init $ tail $ combs 4 [2..13])])
    highCards = map handify highs
    pairs = map handify $ concat [ map ([n,n] ++) (combs 3 (l2h \\ [n])) | n <- l2h]
    twoPairs = map handify $ concat $ concat
               [[[[n,n,m,m,x] | x <- l2h \\ [n,m]]
                       | m <- [2..modRank $ n - 1]]
                       | n <- tail l2h]
    threeOfKinds = map handify $ concat
                   [ map ([n,n,n] ++) (combs 2 (l2h \\ [n])) | n <- l2h]
    conts :: [[Int]]
    conts = concat [[[n..n+4] | n <- [1..9]] ++ [[10,11,12,13,1]]]
    straights = map handify conts
    flushes = map heartify highs
    fullHouses = map handify $ concat [[[n,n,n,m,m] | m <- l2h \\ [n]] | n <- l2h]
    straightFlushes = map heartify $ init conts
    fourOfKinds = map handify $ concat [[[n,n,n,n,m] | m <- l2h \\ [n]] | n <- l2h]
    royalFlush = map heartify [[1, 13, 12, 11, 10]]

group5 = map length $ group $ map checkHand ranking5
expect5 :: [Int]
expect5 = [1277,2860,858,858,10,1277,156,9,156,1]
