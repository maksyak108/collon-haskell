module ColonParser where

import Text.Read (readMaybe)
import Data.List (isPrefixOf, isSuffixOf)

data Command
    = Push Int
    | Pop
    | Add
    | Sub
    | Mul
    | Div
    | Swap
    | Lt
    | Gt
    | Eq
    | If [Command] [Command]
    | DoILoop [Command]
    | Print String
    deriving (Show, Eq)

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

parsePrintCommand :: String -> Maybe String
parsePrintCommand input =
    if ".\"" `isPrefixOf` input && "\"" `isSuffixOf` input
    then Just (drop 2 (init input))  -- Убираем .\" в начале и " в конце
    else Nothing