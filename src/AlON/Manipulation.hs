{-# LANGUAGE ExplicitForAll, FlexibleContexts, RankNTypes, ScopedTypeVariables, TupleSections #-}
module AlON.Manipulation (
    mapDynTreeWithKey
  , mergeDynTree
  , apply2contents
  , foldlDynDynList
  ) where

import Control.Monad
import Data.Text (Text)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import AlON.Source
import Reflex

-- This module is terrible code, nothing has been optimized.
-- This is purely a demonstration implementation.

mapDynTreeWithKey :: (Functor (Dynamic t))
                  => ([Text] -> a -> b) -> DynDirTree t a -> DynDirTree t b
mapDynTreeWithKey f = fmap (LT.mapWithKey' (\p d -> f p <$> d))

mergeDynTree :: Reflex t => DynDirTree t a -> DynDirTree t a
             -> DynDirTree t a
mergeDynTree a b = mconcat $ [a, b]

apply2contents :: (Functor (Dynamic t))
               => (a -> b) -> DynDirTree t a -> DynDirTree t b
apply2contents f = fmap (fmap (fmap f))

foldlDynDynList :: (Reflex t)
                => (b -> a -> b) -> Dynamic t b -> Dynamic t [Dynamic t a] -> Dynamic t b
foldlDynDynList f b0 dld = join $ (foldl (\b a -> f <$> b <*> a) b0) <$> dld

