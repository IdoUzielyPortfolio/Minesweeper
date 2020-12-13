{-# OPTIONS -Wall #-}

module Minesweeper
(
    printNum,
    printRows,
    Cell,
    step,
    firstPlayBoard,
    gameFinish,
    printFinalBoard,
    printFinalBoardCell,
    printFinalCellLst,
    checkFirstArgs,
    makeMaybeIntList,
    makeIntList,
    checkMove,
    checkRowColValid
)where

import System.Random.Shuffle
import System.Random
import Data.Maybe
import Safe
-- data structure for cell
data Cell = Cell{
    mark :: Char,
    num  :: Int,
    isMine :: Bool,
    dig :: Bool,
    flag :: Bool
}deriving (Show,Eq)
-- make from maybe ints list to a int list for checking valid input
makeIntList :: [Maybe Int]->[Int]
makeIntList x = map fromJust x
--check if rows and cols are valid
checkRowColValid ::Maybe Int->Maybe Int->Int->Int->Bool
checkRowColValid x y row col
  | x == Nothing = False
  | y == Nothing = False
  | fromJust x <= 0 || fromJust x > row = False
  | fromJust y <= 0 || fromJust y > col = False
  | otherwise = True 
--check move input is valid
checkMove ::String->Bool
checkMove move 
  | move == "dig" || move == "flag" = True
  | otherwise = False
--take the list of first args and return maybe int if it is valid and nothing if not
makeMaybeIntList :: [String]->[Maybe Int]
makeMaybeIntList x = map readMay x

--check maybe int list and return bool if valid or not
checkFirstArgs ::[Maybe Int] ->Bool
checkFirstArgs input = if (length input) /= 3 then False else argsValidate input

--make step will do action if it is valid move, if you try to dig in flag place there is no action
step :: [[Cell]]->String->Int->Int->Int->Int->[[Cell]]
step board move x y rows cols
  | move == "dig" && ifFlag board x y == True = board
  | move == "flag" = flagFun board x y
  | move == "dig" = digFun board x y rows cols
  | otherwise = board

--function that turn flag on if it is of and turn it of if it is on ,if the cell allready digged no action
flagFun :: [[Cell]]-> Int->Int->[[Cell]]
flagFun board row col
  | (ifFlag board row col == True) = updateBoard board (makeCell ' ' (num (getCell board row col)) (isMine (getCell board row col)) False False) row col
  | (ifDig board row col == True) == True = board
  | otherwise = updateBoard board (makeCell '!' (num (getCell board row col)) (isMine (getCell board row col)) False True) row col
--check if the game is finish
gameFinish ::[[Cell]]->String->Int->Int->(Bool,Bool)
gameFinish board move row col
  | ifMine board row col == True && move == "dig" && ifFlag board row col == False = (True,False)
  | checkBoard board == True = (True,True)
  | otherwise = (False,False)
--check if a certain cell is flaged
ifFlag :: [[Cell]]->Int->Int->Bool
ifFlag board rows cols = flag (getCell board rows cols)
--finish game use this function to check the board status
checkBoard :: [[Cell]]->Bool
checkBoard [] = True
checkBoard (x:xs)
  | checkCellsInRow x == False = False
  | otherwise = checkBoard xs
--check every cell in a certain row the know the game status
checkCellsInRow :: [Cell]->Bool
checkCellsInRow [] = True
checkCellsInRow (x:xs)
  | (isMine x == True && flag x == False) = False
  | (isMine x == False && flag x == True) = False
  | (isMine x == False && flag x == False && dig x == False) = False
  | otherwise = checkCellsInRow xs
--get a cordinate and bring back list of neighbors to dig
neighborsToDig :: Int->Int->Int->Int->[(Int,Int)]
neighborsToDig x y row col = updatelistOfAdjacent (createListOfAdjacent (x,y)) row col
--will apply the dig move, this is a recursive function, while there is number diffrent from zero or a mine or a flaged need to keep diging
digFun :: [[Cell]]->Int->Int->Int->Int->[[Cell]]
digFun board x y rows cols
  | (ifMine board x y) == True = board 
  | (ifDig board x y) == True = board
  | (ifFlag board x y) == True = board
  | num (getCell board x y) /= 0 = updateBoard board (makeCell ' ' (num (getCell board x y)) False True False) x y
  | otherwise = recDig (updateBoard board (makeCell '0' (num (getCell board x y)) False True False) x y) (neighborsToDig x y rows cols) rows cols
--in otherwise we create recursivly the list of neighbors to dig and apply rec dig

--if list of cordinates is empty return board else call digFun with this cell
recDig ::[[Cell]]->[(Int,Int)]->Int->Int->[[Cell]]
recDig board [] _ _ = board
recDig board (x:xs) rows cols = recDig (digFun board (fst x) (snd x) rows cols) xs rows cols

-- create cell
makeCell :: Char->Int->Bool->Bool->Bool-> Cell
makeCell ch number bool choice flaged = Cell{mark = ch ,num = number , isMine = bool, dig = choice , flag = flaged}

--check if certain cell is digged
ifDig :: [[Cell]]->Int->Int->Bool
ifDig board x y = if dig (getCell board x y) == True then True else False

--update col 
updateCol :: [Cell]->Cell->Int->[Cell]
updateCol lst cell col = (take col lst) ++ [cell] ++ (drop (col + 1) lst)

--update board after move (or initial board)
updateBoard :: [[Cell]]->Cell->Int->Int->[[Cell]]
updateBoard lst cell row col = (take (row) lst) ++ [updateCol (lst !! row) cell col] ++ (drop (row + 1) lst)
--creates empty board
createintialBoard :: Int->Int->[[Cell]]
createintialBoard a b = take a (repeat (take b (repeat (makeCell ' ' 0 False False False))))

--put mines in initial board
putMinesInBoard :: Int->Int->Int->[[Cell]]
putMinesInBoard rowNum colNum minesNum = 
  let listMine = generateMines minesNum rowNum colNum
  in putMines (createintialBoard rowNum colNum) 0 minesNum listMine

-- create the first play board
firstPlayBoard :: Int->Int->Int->[[Cell]]
firstPlayBoard rowNum colNum minesNum =
  let b1 =  putMinesInBoard rowNum colNum minesNum
      listMine = generateMines minesNum rowNum colNum
      list = makeListNextToMines b1 listMine rowNum colNum
  in (updateCellNextToMine b1 list)

--prints the upper frame of the game
printNum :: Int->Int->String
printNum colNum limit
  | (colNum + 1) == (limit + 1) = []
  | colNum == 0 = "   00" ++ (show (colNum + 1)) ++ " " ++ printNum (colNum +1) limit
  | (colNum + 1)>= 10 = "0" ++ (show (colNum + 1)) ++ " " ++ printNum (colNum +1) limit
  | otherwise = "00" ++ (show (colNum + 1)) ++ " " ++ printNum (colNum + 1) limit

--prints cell by game defintion
printCell :: Cell->String
printCell cell
  | (mark cell) /= ' ' && (dig cell) /= False = "[" ++ [mark cell] ++ "] "
  | (flag cell) == True = "[" ++ [mark cell] ++ "] "
  | (num cell) /= 0 && (dig cell) /= False = "[" ++ (show (num cell)) ++ "] "
  | otherwise = "[ ] "

--print final board cells
printFinalBoardCell :: Cell->String
printFinalBoardCell cell
  | isMine cell == True = "[*] "
  | flag cell == True = "[!] "
  | (dig cell) /= False = "[" ++ (show (num cell)) ++ "] "
  | otherwise = "[ ] "

--print final board
printFinalBoard :: [[Cell]]->Int->String
printFinalBoard board numRow
  | (numRow + 1) >= 10 = "0" ++ (show (numRow + 1)) ++ printFinalCellLst (board !! numRow)
  | otherwise = "00" ++ (show (numRow + 1)) ++ printFinalCellLst (board !! numRow)

--print every row in final board
printFinalCellLst :: [Cell]->String
printFinalCellLst lst = concat (map printFinalBoardCell lst)

--print all cells in row
printCellLst :: [Cell]->String
printCellLst lst = concat (map printCell lst)

--print board
printRows :: [[Cell]]->Int->String
printRows board numRow
  | (numRow + 1) >= 10 = "0" ++ (show (numRow + 1)) ++ printCellLst (board !! numRow)
  | otherwise = "00" ++ (show (numRow + 1)) ++ printCellLst (board !! numRow)

--make list of random mines
genMines :: Int->[(Int,Int)]->[(Int,Int)]
genMines size lst = take size (System.Random.Shuffle.shuffle' lst (length lst) (mkStdGen size))

--create list of places to put mines
generateMines :: Int->Int->Int->[(Int,Int)]
generateMines size row col = genMines size (makeList2 (makeRange row) (makeRange col)) 

--locate mines in the board
putMines :: [[Cell]]->Int->Int->[(Int,Int)]->[[Cell]]
putMines board numMines maxMines randLoc
  | numMines == maxMines = board
  | otherwise = putMines (updateBoard board (makeCell '*' 0 True False False) (fst (randLoc !! numMines)) (snd (randLoc !! numMines))) (numMines + 1) maxMines randLoc

--create list of number in a range, size of rows or cols
makeRange :: Int->[Int]
makeRange x = [0..(x-1)]

--create list of cordinate in board by range
makeList :: [Int]->[Int]->[(Int,Int)]
makeList _ [] = []
makeList x (y:ys) = [((head x),y)] ++ makeList x ys
-- create the final list of all the cordinate in the board
makeList2 :: [Int]->[Int]-> [(Int,Int)]
makeList2 [] _ = []
makeList2 x y = makeList x y ++ makeList2 (tail x) y

--return cell by index
getCell :: [[Cell]]->Int->Int->Cell
getCell board row col = ((board !! row) !! col)

--update cell next to mine, counter will add one for each mine next to cell
updateCellNextToMine :: [[Cell]]->[(Int,Int)]->[[Cell]]
updateCellNextToMine board [] = board
updateCellNextToMine board (x:xs) =  updateCellNextToMine (updateBoard board (makeCell ' ' (num (getCell board (fst x) (snd x)) + 1) False False False) (fst x) (snd x)) xs

--create list of every 8 options next to specific indexes
createListOfAdjacent:: (Int,Int)->[(Int,Int)]
createListOfAdjacent a = [(((fst a) - 1),((snd a) - 1)),((fst a),((snd a) - 1)),(((fst a) + 1),((snd a) - 1)),(((fst a) - 1 ),(snd a)),(((fst a) + 1 ),(snd a)),(((fst a) - 1),((snd a) + 1)),(((fst a)),(snd a) + 1),(((fst a) + 1),((snd a) + 1))]

--filter the list of all 8 points for valid cordinates 
updatelistOfAdjacent :: [(Int,Int)]->Int->Int->[(Int,Int)]
updatelistOfAdjacent [] _ _ = []
updatelistOfAdjacent (x:xs) rows cols
  | (outOfBounds rows cols x) == True =  [x] ++ updatelistOfAdjacent xs rows cols
  |otherwise = updatelistOfAdjacent xs rows cols
--make the list of all valid possible cordinates next to mines 
makeListNextToMines :: [[Cell]]->[(Int,Int)]->Int->Int->[(Int,Int)]
makeListNextToMines _ [] _ _ = []
makeListNextToMines board (x:xs) row col = finalListOfNeighbors board (updatelistOfAdjacent (createListOfAdjacent x) row col) ++ makeListNextToMines board xs row col

--check that the board doesnt have mines next to each other and update the list if there is
finalListOfNeighbors ::[[Cell]]->[(Int,Int)]->[(Int,Int)]
finalListOfNeighbors _ [] = []
finalListOfNeighbors board (x:xs) = if ifMine board (fst x) (snd x) == True then [] ++ finalListOfNeighbors board xs else [x] ++ finalListOfNeighbors board xs 

--check if certain cell has mine
ifMine :: [[Cell]]->Int->Int->Bool
ifMine board rows cols = if isMine (getCell board rows cols) == True then True else False

--check if the indexes are out of range of the board
outOfBounds   :: Int->Int->(Int,Int)->Bool
outOfBounds  rows cols tup 
  | (fst tup) >= rows = False
  | (snd tup) >= cols = False
  | (fst tup) < 0 = False
  | (snd tup) < 0 =False
  | otherwise =True

--check if the first input args are valid
argsValidate ::[Maybe Int]->Bool
argsValidate args
  | args !! 0 == Nothing = False
  | args !! 1 == Nothing = False
  | args !! 2 == Nothing = False
  | fromJust (args !! 0) > 20 || fromJust (args !! 0) < 10 = False
  | fromJust (args !! 1) > 20 || fromJust (args !! 1) < 10 = False
  | fromJust (args !! 2) > 199 || fromJust (args !! 2) < 4 || fromJust (args !! 2) >= fromJust (args !! 0) * fromJust (args !! 1)  = False
  | otherwise = True

