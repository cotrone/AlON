module ALON.Manipulation (
  ) where

import Reflex.Dynamic


--mapDynM :: forall t m a b. (Reflex t, MonadHold t m) => (forall m'. MonadSample t m' => a -> m' b) -> Dynamic t a -> m (Dynamic t b)

proccessDynTree :: (Reflex t, MonadHold t m) => ([FilePath -> a -> b) -> Dynamic Spider (DirTree (Dynamic Spider a))) -> m (Dynamic Spider (DirTree (Dynamic Spider b))))
proccessDynTree f = mapDynM $ \v -> do
  

--mergeDynTree :: Dynamic Spider (DirTree (Dynamic Spider a))) -> Dynamic Spider (DirTree (Dynamic Spider a)))
--             -> m (Dynamic Spider (DirTree (Dynamic Spider a)))
--mergeDynTree a b = mconcatDyn [a, b]
