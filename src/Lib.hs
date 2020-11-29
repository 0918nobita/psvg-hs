{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Dynamic as DYN
import qualified Data.Map     as M
import           Debug.Trace  (trace)

type Start = Int

type ParserId = Int

data ParserContext = ParserContext
    { source   :: String
    , position :: Int
    , memory   :: M.Map (Start, ParserId) (DYN.Dynamic, Int)
    }
    deriving (Show)

data Parser e a = Parser
    { parserId :: Maybe ParserId
    , parseFn  :: ParserContext -> Either e (a, ParserContext)
    }

char :: Char -> Parser () Char
char c = Parser { parserId = Just 1, parseFn = parse }
    where
        parse :: ParserContext -> Either () (Char, ParserContext)
        parse context =
            let
                src = source context
                pos = position context
            in
            case drop pos src of
                head:_ | head == c ->
                    Right (c, ParserContext { source = src, position = pos + 1, memory = memory context })
                _ -> Left ()

mapParser :: forall a b e . DYN.Typeable a => (a -> b) -> Parser e a -> Parser e b
mapParser f p = Parser { parserId = Nothing, parseFn = parse }
    where
        parse :: ParserContext -> Either e (b, ParserContext)
        parse context = do
            (a, context) <- runParser p context
            return (f a, context)

runParser :: DYN.Typeable a => Parser e a -> ParserContext -> Either e (a, ParserContext)
runParser p context =
    let
        memo = memory context
        src = source context
        start = position context
    in
    case parserId p of
        Just pId ->
            let
                checkCache = do
                    (a, rest) <- M.lookup (start, pId) memo
                    dyn <- DYN.fromDynamic a
                    return (dyn, rest)
            in
            case trace "Checking cache..." checkCache of
                Just (a, tailPos) -> trace "Cache found" Right (a, ParserContext { source = src, position = tailPos, memory = memo })
                Nothing -> do
                    (a, context) <- trace "Parsing..." parseFn p context
                    let pos = position context
                    let newMemo = M.insert (start, pId) (DYN.toDyn a, pos) memo
                    return (a, ParserContext { source = src, position = pos, memory = newMemo })
        Nothing -> parseFn p context

someFunc :: IO ()
someFunc = do
    let context = ParserContext { source = "abc", position = 0, memory = M.empty }
    case runParser (mapParser (: "!") (char 'a')) context of
        Right r -> print r
        Left _  -> putStrLn "Left"
