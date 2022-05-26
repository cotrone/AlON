module AlON.Source (
    TimeBits, utc2TimeBits, afterTime, atTime, time
  , DirTree, DynDirTree
  , dirSource
  , flattenDynDirTree
  , constDynDirTree
  ) where

import Reflex
import Reflex.Filesystem.DirTree
import Reflex.Filesystem.Watch
import Reflex.Time.UTCTime

constDynDirTree :: Reflex t => DirTree a -> DynDirTree t a
constDynDirTree = constDyn . fmap constDyn


flattenDynDirTree :: Reflex t => DynDirTree t a -> Dynamic t (DirTree a)
flattenDynDirTree dirTree = traverse id =<< dirTree
