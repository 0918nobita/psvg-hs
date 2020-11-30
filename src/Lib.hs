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
import           Text.Printf      (printf)

someFunc :: IO ()
someFunc = do
    let p = char 'a'
    print $ parseFn p "abc"
    print $ parseFn (char 'b') "bxx"
    print $ parseFn (p >> char 'b') "abc"

type Memory s a = ST s (STRef s (M.Map String (a, String)))

type ParseFn e a = String -> Either e (a, String)

data Parser e s a = Parser
    { memory  :: Memory s a
    , parseFn :: ParseFn e a
    }

instance Functor (Parser e s) where
    fmap f p = Parser { memory = newSTRef M.empty, parseFn = fmap (B.first f) . parseFn p }

instance Applicative (Parser e s) where
    pure a = Parser { memory = newSTRef M.empty, parseFn = \s -> return (a, s) }

    (<*>) :: forall a b . Parser e s (a -> b) -> Parser e s a -> Parser e s b
    pf <*> pa = Parser { memory = newSTRef M.empty, parseFn = parse }
        where
            parse :: String -> Either e (b, String)
            parse s = do
                (f, rest) <- parseFn pf s
                (a, rest') <- parseFn pa rest
                return (f a, rest')

instance Monad (Parser e s) where
    (>>=) :: forall a b . Parser e s a -> (a -> Parser e s b) -> Parser e s b
    p >>= k = Parser { memory = newSTRef M.empty, parseFn = parse }
        where
            parse :: String -> Either e (b, String)
            parse s = do
                (a, rest) <- parseFn p s
                parseFn (k a) rest

char :: Char -> Parser () s Char
char c =
    let
        memory = newSTRef M.empty
    in
    Parser { memory = memory, parseFn = memorize memory parse }
    where
        parse s = trace (printf "Parsing... %c" c) $
            case s of
                (head:tail) | head == c -> Right (c, tail)
                _                       -> Left ()

memorize :: Ord a => Memory s a -> ParseFn e a -> ParseFn e a
memorize memo f src =
    case M.lookup src $ runST (memo >>= readSTRef) of
        Just a  -> Right a
        Nothing -> do
            r <- f src
            let _ = runST (memo >>= (\m -> modifySTRef m (M.insert src r)))
            return r
