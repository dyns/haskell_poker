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
data CardState = CardState Hands Board Deck deriving Show
 -- data PotState v = PotState (Pot v) deriving Show
data Game v = Game Integer CardState (Pot v) (Players v) deriving Show
data Player v = Player String (Pot v) deriving Show
type Players v = [Player v]

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
    mBets <- askMoneyLoopBlinds game blindStart  
    return game

genNames count = [show x | x <- [1 .. count]]

betOrder players playerCount littleBlind = testBu players littleBlind [] 1 playerCount

testBu players little carry index total | index < little = testBu (drop 1 players) little (carry ++ (take 1 players)) (index + 1) total
    | index == (total + 1) = carry
    | otherwise = (take 1 players) ++ (testBu (drop 1 players) little carry (index + 1) total)

askMoneyLoopBlinds game@(Game count _ _ players0) blindStart = do
        players <- return $ betOrder players0 count blindStart
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
        littleBlind <- askMoney (head players)  " is little blind, min x"
        bigBlind <- askMoney (head $ tail players) " is big blind min (big blind)"
        rest <- (askMoneyLoop (count - 2) players)
        return (littleBlind : bigBlind : rest)

askMoneyLoop count players0 
                                   | count > 0 = do
                        amount <- askMoney player "how much do you want to bet?"
                        rest <- (askMoneyLoop (count - 1) players)
                        return $ amount : rest
                                   | otherwise = do
                                        return []
                        where
                         players = tail players0
                         player = head players0

askMoney player@(Player name (Pot pot)) text = do
    printC name
    printC " "
    print text
    line <- getLine
    amount <- return (read line :: Integer)
    if amount <= pot then
        return amount
    else do
        print "not enough funds" 
        askMoney player text

mk_players count = [ Player "a" pot | pot <- (mk_pots $ fromIntegral count)]

main = do
    randomGen <- getStdGen
    deck <- return $ mk_deck randomGen
    print "how many players?"
    pl <- getLine
    playerCount <- return (read pl :: Integer)

    if playerCount > 22
        then print "max players is 22"
    else do
        game <- return $ Game playerCount (CardState [] [] deck) (Pot 0) (mk_players playerCount)
        game <- return $ deal game
        game <- betRound game 4
        game <- return $ flop game
        game <- return $ turn game
        (Game count (CardState hands board deck) pot players) <- return (river game)
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

