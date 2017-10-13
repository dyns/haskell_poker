module Main where

import Lib
import System.Random.Shuffle
import System.Random
import qualified Data.Map.Strict as Map

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
data Pot v = Pot v deriving (Show, Eq)
type Pots v = [Pot v]
data CardState = CardState Hands Board Deck deriving Show
data Game v = Game Integer CardState (Pot v) (Players v) deriving Show
data Player v = Player String (Pot v) deriving (Show, Eq)
--type Players v = [Player v]
type Players v = Map.Map Integer (Player v)

deal game@(Game count _ _ _) = dealHelper count game

dealHelper count game@(Game playerCount (CardState hands board deck) pot players) | count > 0 = 
    let (hand, deck0) = (take 2 deck, drop 2 deck) 
        in dealHelper (count - 1) (Game playerCount (CardState (hand:hands) board deck0) pot players) --(deal (playerCount - 1)  (Game (hand:hands) board deck))
    | otherwise = game

placeCard cardCount (Game ppcount (CardState hands board deck0) pot players) = let deck = (tail deck0) in Game ppcount (CardState hands (board ++ (take cardCount deck)) (drop cardCount deck)) pot players

flop = placeCard 3
turn = placeCard 1
river = placeCard 1

mk_pots count = replicate count $ Pot 100

bet = 4
littleBet = bet / 2

betRound game blindStart = do
    askMoneyLoopBlinds game blindStart  

genNames count = [show x | x <- [1 .. count]]

--betOrder players playerCount littleBlind = testBu players littleBlind [] 1 playerCount

betOrder blindStart count0 = let count = fromIntegral count0 in [ toInteger x | x <- [blindStart .. count] ++ (take (blindStart - 1) (iterate ((+) 1) 1))]

testBu players little carry index total | index < little = testBu (drop 1 players) little (carry ++ (take 1 players)) (index + 1) total
    | index == (total + 1) = carry
    | otherwise = (take 1 players) ++ (testBu (drop 1 players) little carry (index + 1) total)

askMoneyLoopBlinds game@(Game count _ _ players0) blindStart = do
        order <- return $ betOrder blindStart count 
        printC "this is the order: "
        print order
        game <- askMoney (head order) game " is little blind, min x"
        game <- askMoney (head $ tail order) game " is big blind min (big blind)"
        order <- return (drop 2 order)
        game <- (askMoneyLoop game order)
        return game

askMoneyLoop game [] = do return game
askMoneyLoop game order =  do
                        game <- askMoney (head order) game "how much do you want to bet?"
                        game <- (askMoneyLoop game (tail order))
                        return game

askMoney index game@(Game g1 g2 g3 players) text = do

    player <- return $ Map.lookup index players
    if player == Nothing
        then do
            printC "bad index: "
            printC index
            print "   error: invalid player index"
            return game
    else do
        (Just (Player name (Pot pot))) <- return player 
        printC name
        print text
        line <- getLine
        amount <- return (read line :: Integer)
        if amount <= pot then do
            player3 <- return (Player name (Pot (pot - amount)))
            players3 <- return $ Map.insert index player3 players
            return (Game g1 g2 g3 players3)
        else do
            print "not enough funds, please enter another amount" 
            askMoney index game text

mk_players count = Map.fromList [tp | tp <- zip [1 .. count] [Player (show pIndex) pot | (pIndex, pot) <- zip [1 .. count] (mk_pots $ fromIntegral count) ]]

main = do
    randomGen <- getStdGen
    deck <- return $ mk_deck randomGen
    print "how many players?"
    pl <- getLine
    playerCount <- return (read pl :: Integer)

    if playerCount > 22
        then print "max players is 22"
    else do
        game <- return $ Game playerCount (CardState [] [] deck) (Pot 0) $ mk_players playerCount
        game <- return $ deal game
        game <- betRound game 4
        game <- return $ flop game
        game <- return $ turn game
        (Game count (CardState hands board deck) pot players) <- return (river game)
        print players 


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

