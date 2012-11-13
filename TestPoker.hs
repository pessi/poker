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

-- subsets of size k
combs' 0 _ = [[]]
combs' _ [] = []
combs' k (x:xs)
  | k > length xs + 1 = []
  | otherwise = map (x:) (combs' (k-1) xs) ++ combs' k xs

combs k xs = reverse $ combs' k $ sortBy (flip compare) xs

suits = [Hearts, Spades, Diamonds, Clubs]

handify :: [Int] -> [Card]
handify rs = map (uncurry Card) $ zip (cycle suits) $ map modRank rs

flushify :: [Int] -> [Card]
flushify rs = map (Card Hearts . modRank) rs

modRank :: Int -> Int
modRank n
  | n < 1 = n + 13
  | n > 13 = n - 13
  | otherwise = n

{- there will be 7,462 distinct hands in 5-card poker -}
ranking5 :: [[Card]]
ranking5 = map handify highs ++
       map handify pairs ++
       map handify twoPairs ++
       map handify threeOfKinds ++
       map handify straights ++
       map flushify highs ++
       map handify fullHouses ++
       (map flushify $ init straights) ++
       map handify fourOfKinds ++
       [flushify $ last straights]
  where
    highs :: [[Int]]
    highs = concat ([ map (n :) (init $ combs 4 [2..n-1]) | n <- [7..13]])
            ++ map (1 :) (tail $ init $ combs 4 [2..13])
    pairs = concat [ map ([n,n] ++) (combs 3 ([2..14] \\ [n])) | n <- [2..14]]
    twoPairs = concat $ concat
               [[[[n,n,m,m,x] | x <- [2..14] \\ [n,m]] | m <- [2..n-1]] | n <- [3..14]]
    threeOfKinds = concat
                   [ map ([n,n,n] ++) (combs 2 ([2..14] \\ [n])) | n <- [2..14]]
    straights = take 10 $ map (take 5) $ iterate tail [1..14]
    fullHouses = concat [[[n,n,n,m,m] | m <- [2..14] \\ [n]] | n <- [2..14]]
    fourOfKinds = concat [[[n,n,n,n,m] | m <- [2..14] \\ [n]] | n <- [2..14]]

expect5 :: [(Hand, Int)]
expect5 = [(HighCard, 1277)
          ,(Pair, 2860)
          ,(TwoPairs, 858)
          ,(ThreeOfKind, 858)
          ,(Straight, 10)
          ,(Flush, 1277)
          ,(FullHouse, 156)
          ,(StraightFlush, 9)
          ,(FourOfKind, 156)
          ,(RoyalFlush, 1)]
group5 = map ( \ g -> (head g, length g)) $ group $ map checkHand ranking5
checkOk = group5 == expect5

ranked5 = map rankHand ranking5
rankOk = all (uncurry (<)) $ zip ranked5 $ tail ranked5
