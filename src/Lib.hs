{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Map as M
-- import Debug.Trace (trace)

type Start = Int

type ParserId = Int

data ParserContext a = ParserContext
    { source :: String
    , memory :: M.Map (Start, ParserId) (a, String)
    }
    deriving (Show)

data Parser e a = Parser
    { parserId :: Maybe ParserId
    , parseFn  :: String -> Either e (a, String)
    }

instance Functor (Parser e) where
    fmap :: forall a b . (a -> b) -> Parser e a -> Parser e b
    fmap f p = Parser { parserId = Nothing, parseFn = parse }
        where
            parse :: String -> Either e (b, String)
            parse src = parseFn p src >>= (\(a, rest) -> Right (f a, rest))

runParser :: Parser e a -> ParserContext a -> Start -> Either e ((a, String), ParserContext a)
runParser p context start =
    let
        memo = memory context
        src = source context
    in
    case parserId p of
        Just pId ->
            case M.lookup (start, pId) memo of
                Just success -> Right (success, context)
                Nothing -> do
                    success <- parseFn p $ drop start src
                    let newMemo = M.insert (start, pId) success memo
                    return (success, ParserContext { source = src, memory = newMemo })
        Nothing -> do
            success <- parseFn p $ drop start src
            return (success, context)

data ParsedData = ParsedChar Char
    | ParsedStr String
    deriving (Show)

char :: Char -> Parser () ParsedData
char c = Parser { parserId = Just 1, parseFn = parse }
    where
        parse :: String -> Either () (ParsedData, String)
        parse (head:tail)
            | head == c = Right (ParsedChar c, tail)
            | otherwise = Left ()
        parse _ = Left ()

someFunc :: IO ()
someFunc = do
    case runParser (char 'a') (ParserContext { source = "abc", memory = M.empty }) 0 of
        Right (_, context) ->
            let
                mapper =
                    \case
                        ParsedChar c -> ParsedStr $ c:"!"
                        x            -> x
            in print $ runParser (mapper <$> char 'a') context 0
        Left _ -> putStrLn "Left"
