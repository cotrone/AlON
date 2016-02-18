{-# LANGUAGE ExplicitForAll, FlexibleContexts, RankNTypes, ScopedTypeVariables, TupleSections #-}
module ALON.Manipulation (
    collapse
  , mapDynTreeWithKey
  , mergeDynTree
  ) where

import Control.Monad.Trans
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import Data.Text (Text)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import ALON.Source
import ALON.Types
import Reflex

-- This module is terrible code, nothing has been optimized.
-- This is purely a demonstration implementation.

collapse :: forall t m a b. (Reflex t, MonadHold t m)
         => Int
         -> Dynamic t (DirTree (Dynamic t a))
         -> (forall m'. MonadSample t m' => [Text] -> (DirTree (Dynamic t a))
                                       -> m' (Maybe (DirTree (Dynamic t b))))
         -> m (Dynamic t (DirTree (Dynamic t b)))
collapse d ti f = do
    mapDynM proccess ti
  where
   proccess v = do
     let (vl::[([Text], DirTree (Dynamic t a))]) = head . drop d . iterate children1' $ [([], v)]
     (LT.unions . catMaybes) <$> forM vl (uncurry f)
   children1' :: [([Text], DirTree (Dynamic t a))] -> [([Text], DirTree (Dynamic t a))]
   children1' tl = do
     (p', t') <- tl
     (p'', t'') <- Map.toList . LT.children1 $ t'
     return (p'++[p''], t'')

mapDynTreeWithKey :: (Reflex t, MonadHold t m, MonadIO m, MonadIO (PushM t), MonadIO (PullM t))
                  => (forall m'. (MonadSample t m', MonadIO m') => [Text] -> Dynamic t a -> m' b)
                  -> Dynamic t (DirTree (Dynamic t a))
                  -> m (Dynamic t (DirTree (Dynamic t b)))
mapDynTreeWithKey f = mapDynMIO $ \v -> do
  LT.fromList <$> (forM (LT.toList v) $ \(p, d) -> do
    n <- constDyn <$> f p d
    return (p, n))

mergeDynTree :: (Reflex t, MonadHold t m) => Dynamic t (DirTree (Dynamic t a)) -> Dynamic t (DirTree (Dynamic t a))
             -> m (Dynamic t (DirTree (Dynamic t a)))
mergeDynTree a b = mconcatDyn [a, b]
