module Lib
    ( someFunc
    ) where

import qualified Control.Monad.State.Strict as S
import           Text.Printf

type Parser s e a = S.StateT s (Either e) a

runParser :: Parser s e a -> s -> Either e (a, s)
runParser = S.runStateT

char :: Char -> Parser String () Char
char c = S.StateT parse
    where
        parse :: String -> Either () (Char, String)
        parse (head:tail)
            | head == c = Right (c, tail)
            | otherwise = Left ()
        parse _ = Left ()

abcParser :: Parser String () String
abcParser = do
    a <- char 'a'
    b <- char 'b'
    c <- char 'c'
    return [a, b, c]

someFunc :: IO ()
someFunc =
    let result = runParser abcParser "abcdef"
    in putStrLn
        (case result of
            Right (c, rest) -> printf "Parsed: %s, Rest: %s"  c rest
            Left _          -> "Failed to parse")
