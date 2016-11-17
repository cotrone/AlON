{-# LANGUAGE ExplicitForAll, FlexibleContexts, RankNTypes, ScopedTypeVariables, TupleSections #-}
module ALON.Manipulation (
    mapDynTreeWithKey
  , mergeDynTree
  , apply2contents
  , foldlDynDynList
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Data.Text (Text)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import ALON.Source
import Reflex

-- This module is terrible code, nothing has been optimized.
-- This is purely a demonstration implementation.

mapDynTreeWithKey :: (Functor (Dynamic t))
                  => ([Text] -> a -> b) -> DynDirTree t a -> DynDirTree t b
mapDynTreeWithKey f = fmap (LT.mapWithKey' (\p d -> f p <$> d))

mergeDynTree :: (Reflex t, MonadHold t m) => DynDirTree t a -> DynDirTree t a
             -> m (DynDirTree t a)
mergeDynTree a b = mconcatDyn [a, b]

apply2contents :: (Functor (Dynamic t))
               => (a -> b) -> DynDirTree t a -> DynDirTree t b
apply2contents f = fmap (fmap (fmap f))

foldlDynDynList :: (Reflex t)
                => (b -> a -> b) -> Dynamic t b -> Dynamic t [Dynamic t a] -> Dynamic t b
foldlDynDynList f b0 dld = joinDyn $ (foldl (\b a -> f <$> b <*> a) b0) <$> dld

