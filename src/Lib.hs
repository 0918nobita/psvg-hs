module Lib
    ( someFunc
    ) where

import           Control.Monad.ST (runST)
import qualified Data.Map.Strict  as M
import           Data.STRef       (modifySTRef, newSTRef, readSTRef)
import           Debug.Trace      (trace)

someFunc :: IO ()
someFunc = do
    print $ char 'a' "abc"
    print $ char 'b' "bxx"
    print $ char 'a' "abc"

type Parser e a = String -> Either e (a, String)

char :: Char -> Parser () Char
char c s =
    trace "Parsing..." $
        case s of
            (head:tail) | head == c -> Right (c, tail)
            _                       -> Left ()

memorize :: Ord a => (a -> b) -> a -> b
memorize f =
    let mapRef = newSTRef M.empty in
    \a ->
        case M.lookup a $ runST (mapRef >>= readSTRef) of
            Just b  -> b
            Nothing ->
                let b = f a in
                let _ = runST (mapRef >>= (\m -> modifySTRef m (M.insert a b))) in
                b
