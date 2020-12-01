{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad.ST (runST)
import qualified Data.Bifunctor   as B
import           Data.STRef       (modifySTRef, newSTRef, readSTRef)
import           Debug.Trace      (trace)
import           Text.Printf      (printf)

someFunc :: IO ()
someFunc = do
    print $ runST $ do
        ref <- newSTRef 1
        modifySTRef ref (+ 2)
        readSTRef ref
    {-
    do
        let p = char 'a'
        print $ parseFn p "abc"
        print $ parseFn (char 'b') "bxx"
        print $ parseFn (p >> char 'b') "abc"
    -}

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

char :: Char -> Parser () Char
char c = Parser { parseFn = parse }
    where
        parse s = trace (printf "Parsing... %c" c) $
            case s of
                (head:tail) | head == c -> Right (c, tail)
                _                       -> Left ()
