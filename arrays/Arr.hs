module ForthArrays where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)

-- Тип данных для хранения памяти (массивов)
type Memory = Map Int Int

data ForthState = ForthState { memory :: Memory, nextIndex :: Int }

-- Функция выделения памяти под массив
createArray :: Int -> ForthState -> (Int, ForthState)
createArray size state = 
    let startIndex = nextIndex state
        newMemory = foldl (\mem i -> Map.insert (startIndex + i) 0 mem) (memory state) [0..(size-1)]
    in (startIndex, state { memory = newMemory, nextIndex = startIndex + size })

-- Функция записи значения в массив
writeArray :: Int -> Int -> Memory -> Memory
writeArray index value memory = Map.insert index value memory

-- Функция чтения значения из массива
readArray :: Int -> Memory -> Int
readArray index memory = fromMaybe 0 (Map.lookup index memory)

-- Функция инкрементации значения по индексу
incrementArray :: Int -> Int -> Memory -> Memory
incrementArray index value memory =
    let oldValue = readArray index memory
    in Map.insert index (oldValue + value) memory

-- Пример использования
exampleUsage :: IO ()
exampleUsage = do
    let initialState = ForthState Map.empty 0
        (arrayStart, state1) = createArray 10 initialState
        memory1 = writeArray (arrayStart + 3) 42 (memory state1)
        value = readArray (arrayStart + 3) memory1
    putStrLn $ "Значение в ячейке 3: " ++ show value

    let memory2 = incrementArray (arrayStart + 3) 5 memory1
        value2 = readArray (arrayStart + 3) memory2
    putStrLn $ "Новое значение в ячейке 3 после +5: " ++ show value2
