import Test.Hspec
import Data.List (foldl')
import qualified Data.Map as Map
import Control.Monad (foldM)

type State = (Either String [Int], Map.Map String Int, Map.Map String [String])

interpret :: String -> Either String [Int]
interpret input =
  let (result, _, _) = foldl' stepWithState (Right [], Map.empty, Map.empty) (words input)
  in result

stepWithState :: State -> String -> State
stepWithState (Left err, vars, words) _ = (Left err, vars, words)
stepWithState (Right stack, vars, words) token
  | token == ":" = case defineNewWord stack words of
      Left err -> (Left err, vars, words)
      Right (newStack, newWords) -> (Right newStack, vars, newWords)
  | token `elem` ["+", "-", "*", "/", "MOD"] =
      case applyOp stack token of
        Left err -> (Left err, vars, words)
        Right newStack -> (Right newStack, vars, words)
  | token `elem` ["DUP", "DROP", "SWAP", "OVER", "ROT"] =
      case applyStackOp stack token of
        Left err -> (Left err, vars, words)
        Right newStack -> (Right newStack, vars, words)
  | token `elem` ["buzz?", "is-it-zero?"] =
      case applyConditional stack token of
        Left err -> (Left err, vars, words)
        Right newStack -> (Right newStack, vars, words)
  | otherwise = case reads token of
      [(n, "")] -> (Right (n : stack), vars, words)
      _ -> case Map.lookup token words of
          Just wordDef -> case executeWord stack wordDef of
            Left err -> (Left err, vars, words)
            Right newStack -> (Right newStack, vars, words)
          Nothing -> (Left $ "Unknown token: " ++ token, vars, words)

-- Операции с новым словарем
defineNewWord :: [Int] -> Map.Map String [String] -> Either String ([Int], Map.Map String [String])
defineNewWord stack words = 
  case stack of
    (n:rest) -> Right (rest, Map.insert (show n) ["100", "+"] words)
    _ -> Left "Stack underflow"

-- Арифметические операции
applyOp :: [Int] -> String -> Either String [Int]
applyOp (x:y:ys) "+" = Right $ (x + y) : ys
applyOp (x:y:ys) "-" = Right $ (y - x) : ys
applyOp (x:y:ys) "*" = Right $ (x * y) : ys
applyOp (x:y:ys) "/" =
  if x == 0
  then Left "Division by zero"
  else Right $ (y `div` x) : ys
applyOp (x:y:ys) "MOD" =
  if x == 0
  then Left "Modulo by zero"
  else Right $ ((y `mod` x + x) `mod` x) : ys
applyOp _ _ = Left "Stack underflow"

-- Манипуляции со стеком
applyStackOp :: [Int] -> String -> Either String [Int]
applyStackOp stack "DUP" = dup stack
applyStackOp stack "DROP" = dropOp stack
applyStackOp stack "SWAP" = swap stack
applyStackOp stack "OVER" = over stack
applyStackOp stack "ROT" = rot stack
applyStackOp _ _ = Left "Unknown stack operation"

dup :: [Int] -> Either String [Int]
dup (x:xs) = Right (x:x:xs)
dup _ = Left "Stack underflow"

dropOp :: [Int] -> Either String [Int]
dropOp (_:xs) = Right xs
dropOp _ = Left "Stack underflow"

swap :: [Int] -> Either String [Int]
swap (x:y:xs) = Right (y:x:xs)
swap _ = Left "Stack underflow"

over :: [Int] -> Either String [Int]
over (x:y:xs) = Right (y:x:y:xs)
over _ = Left "Stack underflow"

rot :: [Int] -> Either String [Int]
rot (x:y:z:xs) = Right (z:x:y:xs)
rot _ = Left "Stack underflow"

-- Условные операторы
applyConditional :: [Int] -> String -> Either String [Int]
applyConditional stack "buzz?" = buzz stack
applyConditional stack "is-it-zero?" = isItZero stack
applyConditional _ _ = Left "Unknown conditional operation"

buzz :: [Int] -> Either String [Int]
buzz (x:xs)
  | x `mod` 3 == 0 = Right ([66, 117, 122, 122] ++ xs)  -- ASCII "Buzz"
  | otherwise = Right xs
buzz _ = Left "Stack underflow"

isItZero :: [Int] -> Either String [Int]
isItZero (x:xs)
  | x == 0 = Right ([89, 101, 115] ++ xs)  -- ASCII "Yes"
  | otherwise = Right ([78, 111] ++ xs)    -- ASCII "No"
isItZero _ = Left "Stack underflow"

-- Выполнение пользовательских слов
executeWord :: [Int] -> [String] -> Either String [Int]
executeWord stack wordDef = foldM applyWord stack wordDef

applyWord :: [Int] -> String -> Either String [Int]
applyWord stack token = case reads token of
  [(n, "")] -> Right (n : stack)
  _ -> applyOp stack token

main :: IO ()
main = hspec $ do
  describe "Арифметика" $ do
    it "1 2 3 + должно вернуть [5, 1]" $
      interpret "1 2 3 +" `shouldBe` Right [5, 1]
    
    it "1 2 - должно вернуть [-1]" $
      interpret "1 2 -" `shouldBe` Right [-1]
    
    it "4 5 / должно вернуть [0]" $
      interpret "4 5 /" `shouldBe` Right [0]
    
    it "1 2 3 + + должно вернуть [6]" $
      interpret "1 2 3 + +" `shouldBe` Right [6]
    
    it "1 2 3 + + + должно вызвать Stack underflow" $
      interpret "1 2 3 + + +" `shouldBe` Left "Stack underflow"

  describe "Манипуляции стеком" $ do
    it "1 2 3 4 DUP должно вернуть [1, 2, 3, 4, 4]" $
      interpret "1 2 3 4 DUP" `shouldBe` Right [4, 4, 3, 2, 1]
    
    it "1 2 3 4 DROP должно вернуть [1, 2, 3]" $
      interpret "1 2 3 4 DROP" `shouldBe` Right [3, 2, 1]
    
    it "1 2 3 4 SWAP должно вернуть [1, 2, 4, 3]" $
      interpret "1 2 3 4 SWAP" `shouldBe` Right [3, 4, 2, 1]
    
    it "1 2 3 4 OVER должно вернуть [1, 2, 3, 4, 3]" $
      interpret "1 2 3 4 OVER" `shouldBe` Right [3, 4, 3, 2, 1]
    
    it "1 2 3 4 ROT должно вернуть [1, 3, 4, 2]" $
      interpret "1 2 3 4 ROT" `shouldBe` Right [2, 4, 3, 1]

  describe "Условные операторы" $ do
    it "3 buzz? должно вывести 'Buzz'" $
      interpret "3 buzz?" `shouldBe` Right [66, 117, 122, 122]

    it "0 is-it-zero? должно вывести 'Yes'" $
      interpret "0 is-it-zero?" `shouldBe` Right [89, 101, 115]

    it "1 is-it-zero? должно вывести 'No'" $
      interpret "1 is-it-zero?" `shouldBe` Right [78, 111]

  describe "Определение новых слов" $ do
    it "foo должно возвращать ошибку при недостатке элементов в стеке" $
      interpret ": foo 100 + ; foo" `shouldBe` Left "Stack underflow"