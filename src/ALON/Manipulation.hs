{-# LANGUAGE ExplicitForAll, FlexibleContexts, RankNTypes, ScopedTypeVariables, TupleSections #-}
module ALON.Manipulation (
    collapse
  , mapDynTreeWithKey
  , mergeDynTree
  , apply2contents, apply2contentsM
  , foldlDynDynList
  , mapDynMIO, mapDynMHold
  ) where

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Data.Text (Text)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import ALON.Source
import Reflex

-- This module is terrible code, nothing has been optimized.
-- This is purely a demonstration implementation.

collapse :: forall t m a b. (Reflex t, MonadHold t m)
         => Int
         -> DynDirTree t a
         -> (forall m'. MonadSample t m' => [Text] -> (DirTree (Dynamic t a))
                                       -> m' (Maybe (DirTree (Dynamic t b))))
         -> m (DynDirTree t b)
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

mapDynTreeWithKey :: (Functor (Dynamic t))
                  => ([Text] -> a -> b) -> DynDirTree t a -> DynDirTree t b
mapDynTreeWithKey f = fmap (LT.mapWithKey' (\p d -> f p <$> d))

mergeDynTree :: (Reflex t, MonadHold t m) => DynDirTree t a -> DynDirTree t a
             -> m (DynDirTree t a)
mergeDynTree a b = mconcatDyn [a, b]

apply2contents :: (Functor (Dynamic t))
               => (a -> b) -> DynDirTree t a -> DynDirTree t b
apply2contents f = fmap (fmap (fmap f))

apply2contentsM :: (Reflex t, MonadHold t m, MonadIO (PushM t), MonadIO (PullM t))
               => (forall m'. (MonadSample t m', MonadIO m') => Dynamic t a -> m' (Dynamic t b))
               -> DynDirTree t a -> m (DynDirTree t b)
apply2contentsM trans = mapDynMIO (traverse trans)

foldlDynDynList :: (Reflex t, Applicative (Dynamic t))
                => (b -> a -> b) -> Dynamic t b -> Dynamic t [Dynamic t a] -> Dynamic t b
foldlDynDynList f b0 dld = joinDyn $ (foldl (\b a -> f <$> b <*> a) b0) <$> dld

mapDynMIO :: forall t m a b. (Reflex t, MonadHold t m, MonadIO (PushM t), MonadIO (PullM t)) => (forall m'. (MonadSample t m', MonadIO m') => a -> m' b) -> Dynamic t a -> m (Dynamic t b)
mapDynMIO f d = do
  let e' = push (liftM Just . f :: a -> PushM t (Maybe b)) $ updated d
      eb' = fmap constant e'
      v0 = pull $ f =<< sample (current d)
  bb' :: Behavior t (Behavior t b) <- hold v0 eb'
  let b' = pull $ sample =<< sample bb'
  return $ unsafeDynamic b' e'

mapDynMHold :: forall t m a b
             . (Reflex t, MonadHold t m, MonadHold t (PullM t), MonadFix (PullM t))
            => (forall m'. (MonadSample t m', MonadHold t m', MonadFix m') => a -> m' b) -> Dynamic t a -> m (Dynamic t b)
mapDynMHold f d = do
  let e' = push (liftM Just . f :: a -> PushM t (Maybe b)) $ updated d
      eb' = fmap constant e'
      v0 = pull $ f =<< sample (current d)
  bb' :: Behavior t (Behavior t b) <- hold v0 eb'
  let b' = pull $ sample =<< sample bb'
  return $ unsafeDynamic b' e'
