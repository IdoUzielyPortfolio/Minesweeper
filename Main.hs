{-# OPTIONS -Wall #-} 

module Main where

import System.Environment
import qualified Minesweeper
import Data.Maybe
import Safe
--for every move you are asked to choose an action either "dig" or "flag" and then you need to insert first the row and after the col

-- this function will print the board after every move
printBoard ::[[Minesweeper.Cell]]->Int->Int->Int->Int->IO()
printBoard board row maxRow col maxCol= do
    if row == 0 then putStrLn (Minesweeper.printNum col maxCol) else return()
    if row /= maxRow then putStrLn (Minesweeper.printRows board row) else return()
    if row /= maxRow then printBoard board (row + 1) maxRow col maxCol else return()

--this function will print the final board of the game and reveal all the mines location
printFinalBoard ::[[Minesweeper.Cell]]->Int->Int->Int->Int->IO()
printFinalBoard board row maxRow col maxCol= do
    if row == 0 then putStrLn (Minesweeper.printNum col maxCol) else return()
    if row /= maxRow then putStrLn (Minesweeper.printFinalBoard board row) else return()
    if row /= maxRow then printFinalBoard board (row + 1) maxRow col maxCol else return()
-- this is the recursive function that run the game and get input from user
playGame :: [[Minesweeper.Cell]]->Int->Int->Int->IO()
playGame board rows cols numbersofMines = do
    putStrLn "what is your next move?"
    move <- getLine
    let moveBool = Minesweeper.checkMove move
    --check if the move input is valid (dig or flag)
    if(moveBool == True)
        then do
            putStrLn "choose row "
            xIn <-getLine
            let x = readMay xIn 
            putStrLn "choose col"
            yIn <- getLine
            let y = readMay yIn
            let boolRowCol = Minesweeper.checkRowColValid x y rows cols
            --check if the indexes of rows and cols are valid
            if(boolRowCol == True)
                then do
                    let validX = (fromJust x) - 1
                    let validY = (fromJust y) - 1
                    let gameBoard = Minesweeper.step board move validX validY rows cols
                    printBoard gameBoard 0 rows 0 cols
                    let gameStat = Minesweeper.gameFinish gameBoard move validX validY
                    -- check if the game is finished (lose or win)
                    if (fst gameStat == True) then
                        do 
                            (if snd gameStat == True then do
                                putStrLn "you win! all mines cleared"
                                printBoard gameBoard 0 rows 0 cols
                                 else do
                                     putStrLn "Boom! game is over"
                                     printFinalBoard gameBoard 0 rows 0 cols)
                    else
                        playGame gameBoard rows cols numbersofMines
            else do
                putStrLn "Wrong input not valid index - same step"
                playGame board rows cols numbersofMines
    else do
        putStrLn "Wrong move input - same step "
        playGame board rows cols numbersofMines
-- will get args from run and check if the args are valid, if is valid start game otherwise put error
mainRun ::IO()
mainRun = do
    firstInput <- getArgs
    let inputLst = Minesweeper.makeMaybeIntList firstInput
    let input = Minesweeper.checkFirstArgs inputLst
    let intList = Minesweeper.makeIntList inputLst
    if input == False
        then putStrLn "Wrong Input, row and col between 10-20 , mines number between 4-199 and less then row * col size"
        else do
            let row = (intList !! 0)
            let col = (intList !! 1)
            let mines = (intList !! 2)
            let firstBoard = Minesweeper.firstPlayBoard row col mines
            printBoard firstBoard 0 row 0 col
            playGame firstBoard row col mines
--main
main :: IO()
main = do
    mainRun 
   
    
  
