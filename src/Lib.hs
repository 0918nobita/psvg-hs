{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad.ST (ST, runST)
import qualified Data.Map         as M
import           Data.STRef       (STRef, modifySTRef, newSTRef, readSTRef)

type ParseSuccess a = (a, String)

type ParseResult e a = Either e (ParseSuccess a)

type Memo a = M.Map String (ParseSuccess a)

type Parser e a = String -> ParseResult e a

type MemorizedParser s e a = String -> STRef s (Memo a) -> ST s (ParseResult e a)

memorize :: Parser e a -> MemorizedParser s e a
memorize p src memoRef = do
    memo <- readSTRef memoRef
    case M.lookup src memo of
        Just success -> return (Right success)
        Nothing ->
            case p src of
                Right success ->
                    modifySTRef memoRef (M.insert src success) >> return (Right success)
                Left e        -> return (Left e)

char :: Char -> MemorizedParser s () Char
char c = memorize char'
    where
        char' :: Parser () Char
        char' (head:tail) | head == c = Right (head, tail)
        char' _           = Left ()

someFunc :: IO ()
someFunc = do
    let p = char 'a'
    print $ runST $ do
        memo <- newSTRef M.empty
        p "abc" memo
        p "axx" memo
        p "abc" memo
