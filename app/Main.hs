module Main where

import Lib
import System.Random.Shuffle
import System.Random
import qualified Data.Map.Strict as Map

main :: IO ()

data Suite = Heart | Diamond | Spade | Club deriving (Show, Enum, Eq)
data CardVal = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)
data Card = Card CardVal Suite deriving (Show, Eq)
type Deck = [Card]
type PlayerCount = Integer
type Hands = [Deck]
type Board = Deck
data Pot v = Pot v deriving (Show, Eq)
type Pots v = [Pot v]
data CardState = CardState Board Deck deriving Show
data Game v = Game PlayerCount CardState (Pot v) (Players v) deriving Show
data Player v = Player String Deck (Pot v) deriving (Show, Eq)
type Players v = Map.Map Integer (Player v)

mk_deck_order = pure Card <*> enumFrom Two <*> enumFrom Heart

mk_deck = shuffle' mk_deck_order 52

printC a = putStr . show $ a

deal game@(Game count (CardState board deck) pot players) = (Game count (CardState board (drop (fromIntegral (count * 2)) deck)) pot (Map.fromList (dealHelper (Map.toList players) deck)))

dealHelper (p@(pos, (Player name _ pot)):xs) deck = (pos, (Player name (take 2 deck) pot)) : (dealHelper xs (drop 2 deck))
dealHelper [] deck = []

placeCard cardCount (Game ppcount (CardState board deck0) pot players) = let deck = (tail deck0) in Game ppcount (CardState (board ++ (take cardCount deck)) (drop cardCount deck)) pot players

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
        (Just (Player name hand (Pot pot))) <- return player 
        printC name
        print text
        line <- getLine
        amount <- return (read line :: Integer)
        if amount <= pot then do
            player3 <- return (Player name hand (Pot (pot - amount)))
            players3 <- return $ Map.insert index player3 players
            return (Game g1 g2 g3 players3)
        else do
            print "not enough funds, please enter another amount" 
            askMoney index game text

mk_players names = Map.fromList [tp | tp <- zip [1 .. ] [Player name [] pot | name <- names, let pot = (Pot 100)  ]]

mk_game playerCount names = do
    deck <- mk_random_deck
    return $ Game playerCount (CardState [] deck) (Pot 0) $ mk_players names

mk_random_deck = do
    random <- getStdGen
    return $ mk_deck random

showHands game@(Game _ _ _ players) = mapM_ (\(Player name hand pot) -> do 
    printC "Player: "; printC name; printC " cards: "; print hand;) $ map (\(pos, player) -> player) $ Map.toList players

printBoard state (Game _ (CardState board _) _ _) = do
    printC state
    print board

collectNames total = collectNamesHelp total 1 []

collectNamesHelp total count names | count <= total = do
    printC "Name for player "
    printC count
    print ":"
    name <- getLine
    print ""
    collectNamesHelp total (count + 1) (name:names)
                        | otherwise = do
                            return names
    

main = do
    print "how many players?"
    pl <- getLine
    playerCount <- return (read pl :: Integer)
    if playerCount > 22
        then print "max players is 22"
    else do
        names <- collectNames playerCount
        game <- mk_game playerCount names
        game <- return $ deal game
        showHands game
        --game <- betRound game 4 -- 4 is hard coded, update based on state
        game <- return $ flop game
        printBoard "Flop: " game
        game <- return $ turn game
        printBoard "Turn: " game
        game <- return $ river game
        printBoard "River: " game

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

