{-# LANGUAGE TypeFamilies, RankNTypes #-}

module Lists where

import Prelude hiding ((<*))

import Data.Boolean
import Graphics.GPipe

--takeWhileB :: (IfB a, bool ~ BooleanOf a) => (a -> bool) -> [a] -> [a]
--takeWhileB _ [] = []
--takeWhileB p (x:xs) = ifB (p x) (x : takeWhileB p xs) []
--takeWhileB p x = ifB (p x) x x

--f :: (IfB a, BooleanOf a ~ BooleanOf [a]) => (a -> BooleanOf a) -> a -> a -> [a]
--f p x y = ifB (p x) [x] [y]