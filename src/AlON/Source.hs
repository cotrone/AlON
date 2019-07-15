module AlON.Source (
    TimeBits, utc2TimeBits, afterTime, atTime, time
  , DirTree, DynDirTree
  , dirSource
  , constDynDirTree
  ) where

import Reflex
import Reflex.Filesystem.DirTree
import Reflex.Filesystem.Watch
import Reflex.Time.UTCTime

constDynDirTree :: Reflex t => DirTree a -> DynDirTree t a
constDynDirTree = constDyn . fmap constDyn
