{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module AlON.Types (
    AlONT(unAlON), AlON, MonadAlON(..)
  ) where

import           Data.Text (Text)
import           Control.Monad.Reader
import           Control.Concurrent.EQueue
import           Data.Dependent.Sum (DSum)
import           Data.Functor.Identity
import           Reflex
import           Reflex.Host.Class

newtype AlONT t m a =
    AlON { unAlON :: (ReaderT (STMEQueue (DSum (EventTrigger t) Identity)) (DynamicWriterT t [Text] m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, DynamicWriter t [Text])

instance MonadTrans (AlONT t ) where
  lift = AlON . lift . lift

--instance (Reflex t, Monad m) => MonadReader (TQueue (DSum (EventTrigger t))) (AlONT t m)

instance MonadHold t m => MonadHold t (AlONT t m) where
  hold a = lift . hold a
  holdDyn a = lift . holdDyn a
  holdIncremental a = lift . holdIncremental a
  buildDynamic p = lift . buildDynamic p
  headE = lift . headE

instance MonadSample t m => MonadSample t (AlONT t m) where
  sample = lift . sample

-- !!! EQUeue inside this
instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (AlONT t m) where
  newEventWithTrigger f = lift $ newEventWithTrigger f
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadReadEvent t m => MonadReadEvent t (AlONT t m) where
  readEvent = fmap (fmap lift) . lift . readEvent

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (AlONT t m) where
  subscribeEvent = lift . subscribeEvent

type AlON t a = AlONT t (HostFrame t) a

class (Monad m, MonadIO m, ReflexHost t, MonadFix m, MonadHold t m, MonadSample t m, MonadReflexCreateTrigger t m, DynamicWriter t [Text] m) => MonadAlON t m where
  askEQ :: m (AnyEQueue (DSum (EventTrigger t) Identity))

instance (Monad m, MonadIO m, Reflex t, ReflexHost t, MonadFix m, MonadHold t m, MonadSample t m, MonadReflexCreateTrigger t m) => MonadAlON t (AlONT t m) where
  askEQ = AEQ <$> AlON ask
