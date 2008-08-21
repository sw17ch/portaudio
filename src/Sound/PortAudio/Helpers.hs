module Sound.PortAudio.Helpers where

import Control.Monad
import Control.Monad.Fix

{- Helper functions for this file only. -}
enumToC :: (Enum a, Enum b) => a -> b
enumToC = toEnum . fromEnum

-- | Split a list into as many at most n-lengthed lists as possible.
-- This is useful for interleaving audio channels.
chunk :: Int -> [a] -> [[a]]
chunk n list = case list of
        [] -> []
        (y:ys) -> ch' ys (n - 1) (y:)
    where ch' [] _ k = k [] : []
          ch' (y:ys) 0 k = k [] : ch' ys (n - 1) (y:)
          ch' (y:ys) c k = ch' ys (c - 1) (k . (y:)) 

-- | Continue an action until a boolean condition is false
replicateWhileM :: (Monad m) => m Bool -> m a -> m [a]
replicateWhileM mcond action = let f again = do c <- mcond
                                                if c then liftM2 (:) action again
                                                     else return []
                               in fix f

-- Pulled from Mat Morrow's lambdabot code parser
untilM :: (a -> Bool) -> [a] -> Maybe a
untilM _ [] = Nothing
untilM p (x:xs) = if p x
    then Just x else untilM p xs
