module Main where 
                         

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = a :> [Rose a]
    deriving (Eq, Show)

-- Exercise 1

-- Returns the root of a given Rose tree
root :: Rose a -> a
root (a :> rs)= a 

-- Return the children nodes of a given Rose tree
children :: Rose a -> [Rose a]
children (a :> rs)= rs

-- Returns the number of nodes in a given Rose tree
size :: Rose a -> Int
size (a :> [])= 1
size (a :> rs)= 1 +  sum (map size rs)

-- Return the number of leaves in a given Rose tree
leaves :: Rose a -> Int
leaves (a :> [])= 1 
leaves (a :> rs)= sum (map leaves rs) 

-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"
    
-- Exercise 3

-- Return the next player
nextPlayer :: Player -> Player
nextPlayer currentPlayer  = if currentPlayer == P1 
                                then P2
                            else P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

-- Returns the symbol of a given player
symbol :: Player -> Field
symbol player = if player == P1 
                    then X
                else O