{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Dynamic as DYN
import qualified Data.Map     as M
-- import Debug.Trace (trace)

type Start = Int

type ParserId = Int

data ParserContext = ParserContext
    { source :: String
    , memory :: M.Map (Start, ParserId) (DYN.Dynamic, String)
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

runParser :: DYN.Typeable a => Parser e a -> ParserContext -> Start -> Either e ((a, String), ParserContext)
runParser p context start =
    let
        memo = memory context
        src = source context
    in
    case parserId p of
        Just pId ->
            case M.lookup (start, pId) memo >>= (\(a, rest) -> DYN.fromDynamic a >>= (\dyn -> Just (dyn, rest))) of
                Just success -> Right (success, context)
                Nothing -> do
                    (a, rest) <- parseFn p $ drop start src
                    let newMemo = M.insert (start, pId) (DYN.toDyn a, rest) memo
                    return ((a, rest), ParserContext { source = src, memory = newMemo })
        Nothing -> do
            success <- parseFn p $ drop start src
            return (success, context)

char :: Char -> Parser () Char
char c = Parser { parserId = Just 1, parseFn = parse }
    where
        parse :: String -> Either () (Char, String)
        parse (head:tail)
            | head == c = Right (c, tail)
            | otherwise = Left ()
        parse _ = Left ()

someFunc :: IO ()
someFunc = do
    case runParser (char 'a') (ParserContext { source = "abc", memory = M.empty }) 0 of
        Right (_, context) ->
            print $ runParser ((: "!") <$> char 'a') context 0
        Left _ -> putStrLn "Left"
