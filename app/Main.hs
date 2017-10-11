module Main where

import Lib
import System.Random.Shuffle
import System.Random

main :: IO ()

data Suite = Heart | Diamond | Spade | Club deriving (Show, Enum)

data CardVal = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

data Card = Card CardVal Suite deriving (Show)

type Deck = [Card]

mk_deck_order = pure Card <*> enumFrom Two <*> enumFrom Heart

mk_deck = shuffle' mk_deck_order 52

printC a = putStr . show $ a

{-
ploop count (x:xs) | count > 0 = do
    printC x
    print $ head xs ploop ( (-) count 1 ) (tail xs)
    | otherwise = do
    print ""
-}

type Hands = [Deck]
type Board = Deck

data Game = Game Hands Board Deck deriving Show

deal playerCount game@(Game hands board deck) | playerCount > 0 = 
    let (hand, deck0) = (take 2 deck, drop 2 deck) 
        in deal (playerCount - 1) (Game (hand:hands) board deck0) --(deal (playerCount - 1)  (Game (hand:hands) board deck))
    | otherwise = game

placeCard cardCount (Game hands board deck0) = let deck = (tail deck0) in Game hands (board ++ (take cardCount deck)) (drop cardCount deck)
flop = placeCard 3
turn = placeCard 1
river = placeCard 1

main = do
    randomGen <- getStdGen
    deck <- return $ mk_deck randomGen
    print "how many players?"
    pl <- getLine
    playerCount <- return (read pl :: Int)

    if playerCount > 22
        then print "max players is 22"
    else do
        board <- return (deal playerCount (Game [] [] deck))
        board <- return (flop board)
        board <- return (turn board)
        board <- return (river board)
        print board

{- 
    - select amount of players
        - deal 2 to each
        - flop 3
        - turn 1
        - river 1
        -
        - -}

