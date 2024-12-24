import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Debug.Trace (trace)
import Data.List (isPrefixOf, isSuffixOf)

-- Определяем типы данных
data Command
    = Push Int            -- Добавить элемент на стек
    | Pop                 -- Удалить верхний элемент
    | Add                 -- Сложить два верхних элемента
    | Sub                 -- Вычесть два верхних элемента
    | Mul                 -- Умножить два верхних элемента
    | Div                 -- Разделить два верхних элемента
    | Swap                -- Поменять два верхних элемента местами
    | Lt                  -- Сравнить: меньше
    | Gt                  -- Сравнить: больше
    | Eq                  -- Сравнить: равно
    | If [Command] [Command]  -- Условный оператор (if true then else)
    | DoILoop [Command]     -- Цикл DO I LOOP
    | Print String          -- Печать строки
    deriving (Show, Eq)

data Error
    = EmptyStack       -- Ошибка: пустой стек
    | DivideByZero     -- Ошибка: деление на ноль
    | InsufficientArgs -- Ошибка: недостаточно аргументов
    | InvalidCommand   -- Ошибка: неверная команда
    deriving (Show, Eq)

type Stack = [Int]
type Result = Either Error Stack

-- Интерпретатор для выполнения одной команды
runCommand :: Command -> Stack -> Result
runCommand (Push n) stack = Right (n : stack)
runCommand Pop [] = Left EmptyStack
runCommand Pop (_:xs) = Right xs
runCommand Add (x:y:xs) = Right ((x + y) : xs)
runCommand Add _ = Left InsufficientArgs
runCommand Sub (x:y:xs) = Right ((y - x) : xs)
runCommand Sub _ = Left InsufficientArgs
runCommand Mul (x:y:xs) = Right ((x * y) : xs)
runCommand Mul _ = Left InsufficientArgs
runCommand Div (0:_:_) = Left DivideByZero
runCommand Div (x:y:xs) = Right ((y `div` x) : xs)
runCommand Div _ = Left InsufficientArgs
runCommand Swap (x:y:xs) = Right (y:x:xs)
runCommand Swap _ = Left InsufficientArgs
runCommand Lt (x:y:xs) = Right (if y < x then 1 : xs else 0 : xs)
runCommand Lt _ = Left InsufficientArgs
runCommand Gt (x:y:xs) = Right (if y > x then 1 : xs else 0 : xs)
runCommand Gt _ = Left InsufficientArgs
runCommand Eq (x:y:xs) = Right (if y == x then 1 : xs else 0 : xs)
runCommand Eq _ = Left InsufficientArgs
runCommand (If thenBranch elseBranch) (x:xs)
    | x /= 0    = runProgram thenBranch xs
    | otherwise = runProgram elseBranch xs
runCommand (If _ _) [] = Left EmptyStack
runCommand (DoILoop commands) stack = loop stack
  where
    loop [] = Left EmptyStack
    loop (0:xs) = Right xs  -- Если верхний элемент равен 0, выходим из цикла
    loop s = case runProgram commands s of
        Left err       -> Left err
        Right newStack -> loop newStack
runCommand (Print str) stack = Right (trace str stack)

-- Интерпретатор для выполнения списка команд
runProgram :: [Command] -> Stack -> Result
runProgram [] stack = Right stack
runProgram (cmd:cmds) stack =
    case runCommand cmd stack of
        Left err -> Left err
        Right newStack -> runProgram cmds newStack

-- Функция для чтения и парсинга команды
parseCommand :: String -> Maybe Command
parseCommand str
    | Just text <- parsePrintCommand str = Just (Print text)
    | otherwise = case words str of
        ["Push", n] -> Push <$> readMaybe n
        ["Pop"]     -> Just Pop
        ["Add"]     -> Just Add
        ["Sub"]     -> Just Sub
        ["Mul"]     -> Just Mul
        ["Div"]     -> Just Div
        ["Swap"]    -> Just Swap
        ["Lt"]      -> Just Lt
        ["Gt"]      -> Just Gt
        ["Eq"]      -> Just Eq
        _           -> Nothing

-- Вспомогательная функция для парсинга строкового вывода
parsePrintCommand :: String -> Maybe String
parsePrintCommand input =
    if ".\"" `isPrefixOf` input && "\"" `isSuffixOf` input
    then Just (drop 2 (init input))  -- Убираем .\" в начале и " в конце
    else Nothing

-- Функция для ввода списка команд
readCommands :: IO [Command]
readCommands = do
    putStrLn "Введите команды по одной на строку (пустая строка завершает ввод):"
    readCommandsLoop []
  where
    readCommandsLoop acc = do
        input <- getLine
        if null input
            then return (reverse acc)  -- Завершаем ввод, возвращаем список команд
            else case parseCommand input of
                Just cmd -> readCommandsLoop (cmd : acc)
                Nothing  -> do
                    putStrLn "Неверная команда, попробуйте снова."
                    readCommandsLoop acc

-- Основная функция программы
main :: IO ()
main = do
    commands <- readCommands
    putStrLn "Введите начальный стек (например, [5, 3]):"
    stackInput <- getLine
    let initialStack = fromMaybe [] (readMaybe stackInput)
    case runProgram commands initialStack of
        Left err -> putStrLn $ "Ошибка: " ++ show err
        Right finalStack -> putStrLn $ "Итоговый стек: " ++ show finalStack