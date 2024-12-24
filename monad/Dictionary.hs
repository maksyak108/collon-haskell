import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace (trace)  -- Импортируем trace для отладки

-- Типы ошибок
data Error = WordExists      -- Слово уже существует
           | WordNotFound    -- Слово не найдено
           deriving (Show)

-- Типы команд
data Command = AddWord String String  -- Добавить слово и его определение
             | GetWord String         -- Получить определение слова
             deriving (Show, Eq)

-- Тип данных для хранения стека и словаря
type Stack = [Int]                    -- Стек целых чисел
type Dictionary = Map String String   -- Словарь, где ключ - слово, значение - определение

-- Интерпретатор для выполнения команд
runCommand :: Command -> Stack -> Dictionary -> Either Error Stack
-- Добавление слова в словарь
runCommand (AddWord word definition) stack dict
    | Map.member word dict = Left WordExists  -- Если слово уже существует, ошибка
    | otherwise = Right stack  -- Если добавление прошло успешно, возвращаем стек без изменений

-- Получение слова из словаря
runCommand (GetWord word) stack dict =
    case Map.lookup word dict of
        Just definition -> 
            trace ("Определение слова: " ++ definition) (Right stack)  -- Выводим определение через trace
        Nothing -> Left WordNotFound  -- Если слово не найдено, ошибка

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
                let newDict = Map.insert word (unwords definition) dict
                putStrLn $ "Слово \"" ++ word ++ "\" добавлено!"
                loop newDict
            ("get" : word : _) -> do
                case Map.lookup word dict of
                    Just def -> putStrLn $ "Определение: " ++ def
                    Nothing -> putStrLn "Слово не найдено!"
                loop dict
            ("exit" : _) -> putStrLn "Выход из программы."
            _ -> do
                putStrLn "Неверная команда. Попробуйте ещё раз."
                loop dict

-- Тестирование команд
testCommands :: IO ()
testCommands = do
    let dict = Map.empty :: Dictionary  -- Начинаем с пустого словаря
    -- Пример добавления слова в словарь
    let dictAfterAdd = Map.insert "hello" "Привет" dict
    case runCommand (AddWord "hello" "Привет") [] dictAfterAdd of
        Left err -> putStrLn $ "Ошибка: " ++ show err
        Right stack -> putStrLn $ "Итоговый стек: " ++ show stack

    -- Пример получения слова из словаря
    case runCommand (GetWord "hello") [] dictAfterAdd of
        Left err -> putStrLn $ "Ошибка: " ++ show err
        Right stack -> putStrLn $ "Итоговый стек: " ++ show stack

    -- Пример получения несуществующего слова
    case runCommand (GetWord "world") [] dictAfterAdd of
        Left err -> putStrLn $ "Ошибка: " ++ show err
        Right stack -> putStrLn $ "Итоговый стек: " ++ show stack