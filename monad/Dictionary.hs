import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace (trace)
import Text.Regex.TDFA ((=~))

-- Типы ошибок
data Error = WordExists      -- Слово уже существует
           | WordNotFound    -- Слово не найдено
           | StackMismatch String -- Несоответствие стека
           deriving (Show)

-- Типы команд
data Command = AddWord String String  -- Добавить слово и его определение
             | GetWord String         -- Получить определение слова
             deriving (Show, Eq)

-- Тип данных для хранения стека и словаря
type Stack = [Int]                    -- Стек целых чисел
type Dictionary = Map String String   -- Словарь, где ключ - слово, значение - определение

-- Функция для проверки комментария про стек
parseStackComment :: String -> Either Error (Int, Int)
parseStackComment definition =
    case definition =~ "\\( ([a-z ]*) -- ([a-z ]*) \\)" :: [[String]] of
        [[_, before, after]] -> Right (length (words before), length (words after))
        _ -> Left (StackMismatch "Невозможно проверить комментарий про стек")

-- Интерпретатор для выполнения команд
runCommand :: Command -> Stack -> Dictionary -> Either Error Stack
runCommand (AddWord word definition) stack dict
    | Map.member word dict = Left WordExists
    | otherwise =
        case parseStackComment definition of
            Right _ -> Right stack
            Left err -> Left err

runCommand (GetWord word) stack dict =
    case Map.lookup word dict of
        Just definition -> trace ("Определение слова: " ++ definition) (Right stack)
        Nothing -> Left WordNotFound

interactive :: IO ()
interactive = do
    let dict = Map.empty :: Dictionary
    loop dict
  where
    loop dict = do
        putStrLn "Введите команду (add/get/exit):"
        command <- getLine
        case words command of
            ("add" : word : definition) -> do
                let def = unwords definition
                case parseStackComment def of
                    Right _ -> do
                        let newDict = Map.insert word def dict
                        putStrLn $ "Слово \"" ++ word ++ "\" добавлено!"
                        loop newDict
                    Left err -> do
                        putStrLn $ "Ошибка: " ++ show err
                        loop dict
            ("get" : word : _) -> do
                case Map.lookup word dict of
                    Just def -> putStrLn $ "Определение: " ++ def
                    Nothing -> putStrLn "Слово не найдено!"
                loop dict
            ("exit" : _) -> putStrLn "Выход из программы."
            _ -> do
                putStrLn "Неверная команда. Попробуйте ещё раз."
                loop dict