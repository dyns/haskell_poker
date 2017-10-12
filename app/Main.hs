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

type Hands = [Deck]
type Board = Deck
data Pot v = Pot v deriving Show
type Pots v = [Pot v]

data Game v = Game Hands Board Deck (Pot v) (Pots v) deriving Show

deal playerCount game@(Game hands board deck pot pots) | playerCount > 0 = 
    let (hand, deck0) = (take 2 deck, drop 2 deck) 
        in deal (playerCount - 1) (Game (hand:hands) board deck0 pot pots) --(deal (playerCount - 1)  (Game (hand:hands) board deck))
    | otherwise = game

placeCard cardCount (Game hands board deck0 pot pots) = let deck = (tail deck0) in Game hands (board ++ (take cardCount deck)) (drop cardCount deck) pot pots

flop = placeCard 3
turn = placeCard 1
river = placeCard 1

mk_pots count = replicate count $ Pot 100

bet = 4
littleBet = bet / 2

betRound game@(Game hands board deck pot pots) littleBlind = do
    return game


askMoney2 playerCount littleBlind = askMoneyLoop playerCount  [show x | x <- [1 .. playerCount]] littleBlind False []

{-
askMoneyLoopBlinds count names blindStart = do
        littleBlind <- askMoney ((head names) ++ " (min littleBlind)")
        bigBlind <- askMoney ((head $ tail names) ++ " min (big blind)")
        askMoneyLoop count names
-}


askMoneyLoop count names0 littleBlind blindFound carry
                                   | count > 0 = do
                        amount <- askMoney name
                        rest <- (askMoneyLoop (count - 1) names littleBlind blindFound carry)
                        return $ amount : rest
                                   | otherwise = do
                                        return []
                        where
                         names = tail names0
                         name = head names0

askMoney player = do
    printC player
    print " how much do you want to bet?"
    line <- getLine
    amount <- return (read line :: Integer)
    return amount

main = do
    randomGen <- getStdGen
    deck <- return $ mk_deck randomGen
    print "how many players?"
    pl <- getLine
    playerCount <- return (read pl :: Int)

    if playerCount > 22
        then print "max players is 22"
    else do
        game <- return $ Game [] [] deck (Pot 0) $ mk_pots playerCount
        game <- return $ deal playerCount game
        a <- askMoney2 playerCount playerCount
        print a
        game <- return $ flop game
        game <- return $ turn game
        (Game hands board deck pot pots) <- return (river game)
        print board

{- select amount of players
        - deal 2 to each
        - show hands
        - bet round
        - flop 3
        - bet round
        - turn 1
        - bet round
        - river 1
        - bet round
        - winner gets pot
        -}

