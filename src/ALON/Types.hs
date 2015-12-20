{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module ALON.Types (
    ALONT(unALON), ALON, MonadALON
  , alonLogErrors
  ) where

import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.State

import Control.Concurrent.STM
import Data.Dependent.Sum (DSum)
import Reflex
import Reflex.Host.Class


newtype ALONT t m a =
    ALON { unALON :: (ReaderT (TQueue (DSum (EventTrigger t))) ((StateT (Dynamic t [Text])) m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadState (Dynamic t [Text]), MonadReader (TQueue (DSum (EventTrigger t))))

instance MonadTrans (ALONT t ) where
  lift = ALON . lift . lift

instance MonadHold t m => MonadHold t (ALONT t m) where
  hold a = lift . hold a

instance MonadSample t m => MonadSample t (ALONT t m) where
  sample = lift . sample

type ALON t a = ALONT t (HostFrame t) a

class (Monad m, ReflexHost t, MonadHold t m, MonadSample t m) => MonadALON t m where
  alonLogErrors :: Dynamic t [Text] -> m ()

instance (Reflex t, ReflexHost t, MonadHold t m, MonadSample t m) => MonadALON t (ALONT t m) where
  alonLogErrors ne = do
    get >>= (mconcatDyn . (:[ne])) >>= put
