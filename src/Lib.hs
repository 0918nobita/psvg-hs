module Lib
    ( someFunc
    ) where

import           Control.Monad              (forM_)
import           Control.Monad.ST           (runST)
import           Control.Monad.State.Strict (StateT (..), runStateT)
import           Data.STRef                 (modifySTRef, newSTRef, readSTRef)
import           Debug.Trace                (traceShow)
import           Text.Printf                (printf)

type Parser s e a = StateT s (Either e) a

runParser :: Parser s e a -> s -> Either e (a, s)
runParser = runStateT

char :: Char -> Parser String () Char
char c = StateT parse
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

sum' :: [Int] -> Int
sum' xs = runST $ do
    ref <- newSTRef 0
    forM_ xs $ \i -> traceShow i modifySTRef ref (+ i)
    readSTRef ref

someFunc :: IO ()
someFunc = do
    let result = runParser abcParser "abcdef"
    putStrLn $
        case result of
            Right (c, rest) -> printf "Parsed: %s, Rest: %s"  c rest
            Left _          -> "Failed to parse"
    print $ sum' [0..100]
