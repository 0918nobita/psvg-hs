module Lib
    ( someFunc
    ) where

import           Control.Monad    (forM_)
import           Control.Monad.ST (runST)
import qualified Data.Map         as M
import           Data.STRef       (modifySTRef, newSTRef, readSTRef)
import           Debug.Trace      (trace)

type Start = Int

type ParserId = Int

data ParserContext a = ParserContext
    { source :: String
    , memory :: M.Map (Start, ParserId) (a, String)
    }
    deriving (Show)

data Parser e a = Parser
    { parserId :: ParserId
    , parseFn  :: String -> Either e (a, String)
    }

runParser :: Parser e a -> ParserContext a -> Start -> Either e ((a, String), ParserContext a)
runParser p context start =
    let
        memo = memory context
        pId = parserId p
        src = source context
    in
    case M.lookup (start, pId) memo of
        Just success -> trace "Cache found" Right (success, context)
        Nothing -> do
            success <- trace "Parsing..." parseFn p $ drop start src
            let newMemo = M.insert (start, pId) success memo
            return (success, ParserContext { source = src, memory = newMemo })

char :: Char -> Parser () Char
char c = Parser { parserId = 1, parseFn = parse }
    where
        parse :: String -> Either () (Char, String)
        parse (head:tail)
            | head == c = Right (c, tail)
            | otherwise = Left ()
        parse _ = Left ()

sum' :: [Int] -> Int
sum' xs = runST $ do
    ref <- newSTRef 0
    forM_ xs $ modifySTRef ref . (+)
    readSTRef ref

someFunc :: IO ()
someFunc = do
    case runParser (char 'a') (ParserContext { source = "abc", memory = M.empty }) 0 of
        Right (_, context) ->
            print $ runParser (char 'a') context 0
        Left _ -> putStrLn "Left"
    print $ sum' [0..100]
