import Poker

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

results = map checkHand hands
