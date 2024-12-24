module Main where

import ColonParser
import Control.Monad (when)
import System.IO
import System.Environment (getArgs)
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf, isSuffixOf)

data Result = LeftError String | RightResult [Int] deriving Show

runCommand :: Command -> [Int] -> Result
runCommand (Push n) stack = RightResult (n : stack)
runCommand Pop [] = LeftError "Empty stack"
runCommand Pop (_:xs) = RightResult xs
runCommand Add (x:y:xs) = RightResult ((x + y) : xs)
runCommand Add _ = LeftError "Insufficient arguments"
runCommand Sub (x:y:xs) = RightResult ((y - x) : xs)
runCommand Sub _ = LeftError "Insufficient arguments"
runCommand Mul (x:y:xs) = RightResult ((x * y) : xs)
runCommand Mul _ = LeftError "Insufficient arguments"
runCommand Div (0:_:_) = LeftError "Divide by zero"
runCommand Div (x:y:xs) = RightResult ((y `div` x) : xs)
runCommand Div _ = LeftError "Insufficient arguments"
runCommand Swap (x:y:xs) = RightResult (y:x:xs)
runCommand Swap _ = LeftError "Insufficient arguments"
runCommand Lt (x:y:xs) = RightResult (if y < x then 1 : xs else 0 : xs)
runCommand Lt _ = LeftError "Insufficient arguments"
runCommand Gt (x:y:xs) = RightResult (if y > x then 1 : xs else 0 : xs)
runCommand Gt _ = LeftError "Insufficient arguments"
runCommand Eq (x:y:xs) = RightResult (if y == x then 1 : xs else 0 : xs)
runCommand Eq _ = LeftError "Insufficient arguments"
runCommand (If thenBranch elseBranch) (x:xs)
    | x /= 0    = runProgram thenBranch xs
    | otherwise = runProgram elseBranch xs
runCommand (If _ _) [] = LeftError "Empty stack"
runCommand (DoILoop commands) stack = loop stack
  where
    loop [] = LeftError "Empty stack"
    loop (0:xs) = RightResult xs
    loop s = case runProgram commands s of
        LeftError err -> LeftError err
        RightResult newStack -> loop newStack
runCommand (Print str) stack = RightResult stack

runProgram :: [Command] -> [Int] -> Result
runProgram [] stack = RightResult stack
runProgram (cmd:cmds) stack =
    case runCommand cmd stack of
        LeftError err -> LeftError err
        RightResult newStack -> runProgram cmds newStack

-- Функция для удаления комментариев (однострочные и многострочные)
removeComments :: String -> String
removeComments input = unlines $ map removeSingleLineComment $ lines input
  where
    removeSingleLineComment line
        | "--" `isPrefixOf` line = ""  -- Убираем однострочные комментарии
        | "{-" `isPrefixOf` line = ""  -- Начало многострочного комментария
        | "-}" `isSuffixOf` line = ""  -- Конец многострочного комментария
        | otherwise = line

-- Чтение команд из файла, игнорируя комментарии
readCommandsFromFile :: FilePath -> IO [Command]
readCommandsFromFile filePath = do
    contents <- readFile filePath
    let cleanedContents = removeComments contents  -- Убираем комментарии
    let linesOfCommands = lines cleanedContents
    return (mapMaybe parseCommand linesOfCommands)

-- Чтение стека из файла
readStackFromFile :: FilePath -> IO [Int]
readStackFromFile filePath = do
    contents <- readFile filePath
    return (read contents :: [Int])

-- Основная программа
main :: IO ()
main = do
    -- Получаем аргументы командной строки
    args <- getArgs

    -- Проверяем, что аргументов два
    if length args /= 2
        then putStrLn "Нужно передать два файла: первый с командами, второй — с начальным стеком"
        else do
            let commandsFile = head args
            let stackFile = last args

            -- Чтение команд и стека из файлов
            commands <- readCommandsFromFile commandsFile
            initialStack <- readStackFromFile stackFile

            -- Выполнение программы
            case runProgram commands initialStack of
                LeftError err -> putStrLn $ "Error: " ++ err
                RightResult finalStack -> putStrLn $ "Final stack: " ++ show finalStack