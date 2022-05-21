{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RankNTypes, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module AlON.Types (
    AlONT(unAlON), AlON, MonadAlON(..), AnyContent(..), AlONContent(..), AlONHostT(unAlONHostT)
  ) where

import           Control.Monad.Reader
import           Control.Concurrent.EQueue
import           Data.ByteString.Lazy (ByteString)
import           Data.Dependent.Sum (DSum)
import           Data.Functor.Identity
import           Data.Text (Text)
import           Data.Kind (Type)
import qualified Network.HTTP.Types as HTTP
import           Reflex
--import           Reflex.Filesystem.DirTree
import           Reflex.Host.Class

class AlONContent a where
  alonContentStatus  :: a -> HTTP.Status
  alonContentHeaders :: a -> HTTP.ResponseHeaders
  alonContentBody    :: a -> ByteString

data AnyContent
  = forall a. (AlONContent a, Show a) => AnyContent a

deriving instance Show AnyContent

instance AlONContent AnyContent where
  alonContentStatus  (AnyContent c) = alonContentStatus c
  alonContentHeaders (AnyContent c) = alonContentHeaders c
  alonContentBody    (AnyContent c) = alonContentBody c

--genericizeContentTree :: AlONContent c => c -> AnyContent
--genericizeContentTree = AnyContent

newtype AlONT t m a =
    AlON { unAlON :: (ReaderT (STMEQueue (DSum (EventTrigger t) Identity)) (DynamicWriterT t [Text] m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, DynamicWriter t [Text])

newtype AlONHostT t (m :: Type -> Type) a = AlONHostT {
  unAlONHostT :: DynamicWriterT t [Text] (PerformEventT t m) a
} deriving (Functor, Applicative, Monad, MonadFix, DynamicWriter t [Text])

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
