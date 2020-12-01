{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad.ST (ST, runST)
import qualified Data.Bifunctor   as B
import qualified Data.Map         as M
import           Data.STRef       (STRef, modifySTRef, newSTRef, readSTRef)
import           Debug.Trace      (trace)

type ParseSuccess a = (a, String)

type ParseResult e a = Either e (ParseSuccess a)

type Memo a = M.Map String (ParseSuccess a)

char :: Char -> String -> STRef s (Memo Char) -> ST s (ParseResult () Char)
char c src memoRef = do
    memo <- readSTRef memoRef
    trace ("Parsing " ++ src ++ " ...") $
        case M.lookup src memo of
            Just success -> trace "  Cache found!" return (Right success)
            Nothing ->
                trace "  Cache not found" $ case src of
                    head:tail | head == c ->
                        let r = (head, tail) in
                        modifySTRef memoRef (M.insert src r) >> return (Right r)
                    _                     -> return (Left ())

someFunc :: IO ()
someFunc = do
    let p = char 'a'
    print $ runST $ do
        memo <- newSTRef M.empty
        p "abc" memo
        p "axx" memo
        p "abc" memo


newtype Parser e a = Parser { parseFn :: String -> Either e (a, String) }

instance Functor (Parser e) where
    fmap f p = Parser { parseFn = fmap (B.first f) . parseFn p }

instance Applicative (Parser e) where
    pure a = Parser { parseFn = \s -> return (a, s) }

    (<*>) :: forall a b . Parser e (a -> b) -> Parser e a -> Parser e b
    pf <*> pa = Parser { parseFn = parse }
        where
            parse :: String -> Either e (b, String)
            parse s = do
                (f, rest) <- parseFn pf s
                (a, rest') <- parseFn pa rest
                return (f a, rest')

instance Monad (Parser e) where
    (>>=) :: forall a b . Parser e a -> (a -> Parser e b) -> Parser e b
    p >>= k = Parser { parseFn = parse }
        where
            parse :: String -> Either e (b, String)
            parse s = do
                (a, rest) <- parseFn p s
                parseFn (k a) rest
