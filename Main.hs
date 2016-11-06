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

-- | Move generation

-- Returns a list of rows that occur after a move in that row
moveInRow :: Row -> Field -> [Row]
moveInRow (x,y,z) symbol 
    | x==B && y==B && z==B = [(symbol,y,z),(x,symbol,z),(x,y,symbol)]
    | x==B && y==B = [(symbol,y,z),(x,symbol,z)]
    | x==B && z==B = [(symbol,y,z),(x,y,symbol)]
    | z==B && y==B = [(x,symbol,z),(x,y,symbol)]
    | x==B = [(symbol,y,z)]
    | y==B = [(x,symbol,z)]
    | z==B = [(x,y,symbol)]
    | otherwise = []

-- Returns a Board list that includes all possible boards after one move
createBoards :: Board -> Int -> Field -> [Board]
createBoards (x,y,z) rowNumber symbol
    | rowNumber == 1 = makeList (moveInRow x symbol) y z
    | rowNumber == 2 = makeList (moveInRow y symbol) x z
    | rowNumber == 3 = makeList (moveInRow z symbol) x y
    where
        makeList (row:rs) row1 row2 
            | rowNumber == 1 = (row,row1,row2) : makeList rs row1 row2
            | rowNumber == 2 = (row1,row,row2) : makeList rs row1 row2
            | rowNumber == 3 = (row1,row2,row) : makeList rs row1 row2
        makeList [] row1 row2 = []

-- Returns list of the boards after a possible move
moves :: Player -> Board -> [Board]
moves  player board = let sym = symbol player in
    createBoards board 1 sym  ++ createBoards board 2 sym ++ createBoards board 3 sym
    

-- | Gametree generation

-- Checks if the game is done or not. 
isDone :: [Row] -> Maybe Player
isDone [] = Nothing
isDone ((x,y,z):rs) 
    | x==X && y==X && z==X = Just P1
    | x==O && y==O && z==O = Just P2
    | otherwise = isDone rs

-- Checks if the game is over or not. If the is ended, returns the winner.
hasWinner :: Board -> Maybe Player
hasWinner board = let allRows = convertToList board ++ convertToList (verticals board) ++ diagonalsToList (diagonals board) in
    isDone allRows
    where 
        convertToList (r1,r2,r3) = [r1,r2,r3]
        diagonalsToList (r1,r2) = [r1,r2]

-- Returns the game tree 
gameTree :: Player -> Board -> Rose Board
gameTree player board
    | isJust (hasWinner board) = board :> []
    | null (moves player board) = board :> []
    | tail (moves player board) /= [] = board :> generateTree (nextPlayer player) (moves player board)
    | otherwise = board :> [head (moves player board) :> []]

-- Creates a tree list recursively. It is used for the children list of a node
generateTree :: Player -> [Board] -> [Rose Board]
generateTree _ [] = []
generateTree player (board:rs) = root (gameTree player board) :> children (gameTree player board) : generateTree player rs

-- Returns the number of leaves in a game tree
gameTreeComplexity :: Int
gameTreeComplexity = leaves (gameTree P1 emptyBoard)

-- | Minimax

-- Minimax method to minimize the score of the player that we don't want to lose. It plays like opponent player 
minimax' :: Player -> Rose Board -> Rose Int
-- If the argument is a leaf, returns the score
minimax' player (b :> []) 
    | hasWinner b == Just player = (-1):>[]     
    | hasWinner b == Nothing = 0:>[]
    | otherwise = 1:>[]
minimax' player board@(b:>bs) = (minimum'  (map root _minimax ) :> minimaxChildren bs )
    where
        next = nextPlayer player
        -- Applies the moves of next player on children nodes
        _minimax = map (minimax next) bs 
        -- Returns a list of children after a move
        minimaxChildren [] = []
        minimaxChildren boardList =  map makeRose boardList
        -- All boards in the list converted to a tree
        makeRose board = root (minimax next board) :> children (minimax next board)
 
-- Minimax method to maximize the score of a given player        
minimax :: Player -> Rose Board -> Rose Int
-- If the argument is a leaf, returns the score
minimax player (b:>[]) 
    | hasWinner b == Just player = 1:>[]
    | isNothing (hasWinner b) = 0:>[]
    | otherwise = (-1):>[]
minimax player board@(b:>bs) = (maximum'  (map root _minimax) :> minimaxChildren bs )
    where 
        next = nextPlayer player
        -- Applies the moves of next player on children nodes
        _minimax = map (minimax' next) bs
        -- Returns a list of children after a move
        minimaxChildren [] = []
        minimaxChildren boardList = map  makeRose boardList
        -- All boards in the list converted to a tree
        makeRose board = root (minimax' next board) :> children (minimax' next board)

-- * Lazier minimum and maximums

-- Lazy minimum method which stops searchin when hits -1
minimum' :: [Int] -> Int
minimum' [x] = x 
minimum' (x:xs) 
    | x == (-1) = (-1)
    | x < minValue = x
    | otherwise = minValue
    where 
        minValue = minimum' xs

-- Lazy maximum method which stops searchin when hits 1
maximum' :: [Int] -> Int
maximum' [x] = x 
maximum' (x:xs) 
    | x == 1 = 1
    | x > maxValue = x
    | otherwise = maxValue
    where
        maxValue = maximum' xs
