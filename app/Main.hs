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

betRound game@(Game hands board deck pot pots) count names blindStart = do
    mBets <- askMoneyLoopBlinds count names blindStart
    printC "bets: "
    print mBets
    return game

genNames count = [show x | x <- [1 .. count]]

betOrder names playerCount littleBlind = testBu names littleBlind [] 1 playerCount

testBu names little carry index total | index < little = testBu (drop 1 names) little (carry ++ (take 1 names)) (index + 1) total
    | index == (total + 1) = carry
    | otherwise = (take 1 names) ++ (testBu (drop 1 names) little carry (index + 1) total)

askMoneyLoopBlinds count names0 blindStart = do
        names <- return $ betOrder names0 count blindStart
        {-
        printC "count "
        print count
        printC "blindStart "
        print blindStart
        printC "orig names "
        print names0
        printC "reorder names: "
        print names
        -}
        littleBlind <- askMoney ((head names) ++ " (min littleBlind)")
        bigBlind <- askMoney ((head $ tail names) ++ " min (big blind)")
        rest <- (askMoneyLoop (count - 2) names)
        return (littleBlind : bigBlind : rest)

askMoneyLoop count names0 
                                   | count > 0 = do
                        amount <- askMoney name
                        rest <- (askMoneyLoop (count - 1) names)
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
        game <- betRound game playerCount (genNames playerCount) 4
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

