module ALON.Types (
    alonLogErrors
  ) where

import Data.Text (Text)
import Reflex

alonLogErrors :: (Reflex t, MonadHold t m) => Dynamic t [Text] -> m ()
alonLogErrors = const $ return ()
