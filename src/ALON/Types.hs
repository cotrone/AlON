{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module ALON.Types (
    ALONT(unALON), ALON, MonadALON(..)
  ) where

import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.State

import Control.Concurrent.STM
import Data.Dependent.Sum (DSum)
import Data.Functor.Identity
import Reflex
import Reflex.Host.Class

newtype ALONT t m a =
    ALON { unALON :: (ReaderT (TQueue (DSum (EventTrigger t) Identity)) ((StateT (Dynamic t [Text])) m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadState (Dynamic t [Text]))

instance MonadTrans (ALONT t ) where
  lift = ALON . lift . lift

--instance (Reflex t, Monad m) => MonadReader (TQueue (DSum (EventTrigger t))) (ALONT t m)

instance MonadHold t m => MonadHold t (ALONT t m) where
  hold a = lift . hold a
  holdDyn a = lift . holdDyn a
  holdIncremental a = lift . holdIncremental a

instance MonadSample t m => MonadSample t (ALONT t m) where
  sample = lift . sample

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ALONT t m) where
  newEventWithTrigger f = lift $ newEventWithTrigger f
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

type ALON a = forall t. Reflex t => ALONT t (HostFrame t) a

class (Monad m, MonadIO m, ReflexHost t, MonadFix m, MonadHold t m, MonadSample t m, MonadReflexCreateTrigger t m) => MonadALON t m where
  alonLogErrors :: Dynamic t [Text] -> m ()
  askEQ :: m (TQueue (DSum (EventTrigger t) Identity))

instance (Monad m, MonadIO m, Reflex t, ReflexHost t, MonadFix m, MonadHold t m, MonadSample t m, MonadReflexCreateTrigger t m) => MonadALON t (ALONT t m) where
  alonLogErrors ne = do
    get >>= (mconcatDyn . (:[ne])) >>= put
  askEQ = ALON ask
