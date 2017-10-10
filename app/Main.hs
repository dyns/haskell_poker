module Main where

import Lib

main :: IO ()

data Suite = Heart | Diamond | Spade | Club
    deriving (Show, Enum)

data CardVal = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
   deriving (Show, Eq, Ord, Enum)

data Card = Card CardVal Suite
    deriving (Show)

type Deck = [Card]

mk_deck = pure Card <*> enumFrom Two <*> enumFrom Heart

main = do
    print mk_deck

