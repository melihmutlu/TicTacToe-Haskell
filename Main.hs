module Main where 
                         

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = a :> [Rose a]
    deriving (Eq, Show)

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

-- Returns the symbol of a given player
symbol :: Player -> Field
symbol player = if player == P1 
                    then X
                else O

-- Returns columns of a given board
verticals :: Board -> (Row, Row, Row)
verticals ((x1,x2,x3),(y1,y2,y3),(z1,z2,z3)) = ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) 

-- Return the diagonals of a given board
diagonals :: Board -> (Row, Row)
diagonals ((x1,_,x3),(_,y2,_),(z1,_,z3))= ((x1,y2,z3),(x3,y2,z1))

-- Creates an empty board
emptyBoard :: Board
emptyBoard = ((B,B,B),(B,B,B),(B,B,B))

-- Pritns the board in a proper way
printBoard :: Board -> String
printBoard (r1,r2,r3)= printRow r1 ++ "\n-+-+-\n" ++ printRow r2 ++ "\n-+-+-\n" ++ printRow r3 ++ "\n"
    where 
        printRow (x,y,z) = show x ++ "|" ++ show y ++ "|" ++ show z 
