{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module ALON.Source (
    TimeBits, utc2TimeBits, afterTime, atTime, time
  , DirTree, DynDirTree, DataUpdate(..)
  , dirSource
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Data.ListTrie.Patricia.Map.Ord (TrieMap)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified System.FilePath as FP
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($=), ($$))
import qualified Data.Conduit.List as CL
import Data.Conduit.Combinators (sourceDirectoryDeep)
import Data.Functor.Misc
import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time
import Data.Text (Text)
import Data.Functor.Identity
import qualified GHC.Event as GHC
import Data.Bits
import Data.Time.Clock.POSIX

import ALON.Types
import ALON.Source.Internal

-- | TimeBits is a list, each step containing the value of the time since the POSIX epoc,
--   shifted right one more then the last.
--   By using this, one can wait on the time changing more then a desired picosecond*2^Nat amount.
--   To get a precise wait, one may then have to wait again on a finer time counter.
--   The head of the list stores the full time as picoseconds since the unix epoch.
type TimeBits t = (Dynamic t UTCTime, [Dynamic t Integer])

-- | Create a (lazy) list of Dynamics, each holding a the value of the UTCTime right-shifted as many bits
--   as there are elements before it in the list.
utc2TimeBits :: forall t. (Functor (Dynamic t)) => Dynamic t UTCTime -> TimeBits t
utc2TimeBits dt = (dt, [timeSlice b <$> dt | b <- [0..]])

-- | Get the value of the UTCTime right-shifted the specified number of bits.
--   The conversion to Integer could be shared but would prevent code reuse.
timeSlice :: Int -> UTCTime -> Integer
timeSlice b = (`shiftR` b) . floor . (* 10^(12::Int)) . utcTimeToPOSIXSeconds

-- | Efficiently compare a target time with a timebits, becoming True at the chosen time.
--   O(log2 (then - now)) tests against the time while waiting to become true.
--
--   We find a list of the postfixs of the time that show the target time isn't already past.
--   The list should be the length of the log of the difference of the times, making this efficient.
--
--   This should be leap second resilient.
afterTime :: forall t m. (Reflex t, MonadSample t m, MonadHold t m, MonadFix m) => TimeBits t -> UTCTime -> m (Dynamic t Bool)
afterTime (curTime, tbs) tgt = do
    n <- sample . current $ curTime
    case tgt <= n of
      True -> return . constDyn $ True
      False -> do
        prefix <- dissimilarPrefix 0 [] tbs
        -- onceE to avoid reprocessing once True.
        holdDyn False =<< onceE =<< (allGreater $ prefix)
  where
    -- Check that each step of the time is now greater then or equal to the time,
    -- switching promptly to the next step in the chain each time,
    -- and firing  when the time is >=.
    -- All finer events fire when a corser event fires, so if we not are >= a more precise event, we'll
    -- pass thought it on this step, and go directly to the next we're not >= to.
    allGreater :: [Event t Integer] -> m (Event t Bool)
    allGreater [] =
      -- Since TimeBits changed, the curTime component had to fire.
      -- Thus we can generatea firing event off it.
      -- Since we got here, we know nothing in the list differs so we're definately True.
      return . fmap (const True) . updated $ curTime
    -- Process each potential difference one step at a time, moving to the next step when it has become True.
    allGreater (cstep:t) = do
      -- We need what the event for the next step would be.
      nstep <- allGreater t
      -- whenever this step fires, we want to become that next step.
      switchPromptly never . fmap (const nstep) $ cstep

    -- Find the prefix of time bits to a time >=
    dissimilarPrefix :: (Reflex t, MonadSample t m1)
                     => Int -> [Event t Integer] -> [Dynamic t Integer] -> m1 [Event t Integer]
    dissimilarPrefix _ _ [] = error "Unpossible, its an infinite list!"
    dissimilarPrefix step pre (cur:rest) = do
      thisStep <- sample . current $ cur
      let whatThisStepShouldBe = timeSlice step tgt
      if thisStep >= whatThisStepShouldBe
        then return pre
        else 
        let fireWhenTargetOrGreater = ffilter (\tStep -> tStep >= whatThisStepShouldBe) . updated $ cur
        in dissimilarPrefix (step+1) (fireWhenTargetOrGreater:pre) rest

-- | Efficiently fires an event at the target time, or immediately after.
--   Fires instantly if the time is already past.
--   
--   Does not take into account leap seconds.
atTime :: (MonadALON t m)
        => UTCTime -> m (Event t ())
atTime theTime = do
  eq <- askEQ
  e <- newEventWithTrigger $ \et -> do
    tm <- GHC.getSystemTimerManager
    now <- getCurrentTime
    tk <- GHC.registerTimeout tm (max 0 . ceiling $ (theTime `diffUTCTime` now)*(10^(6::Int))) (atomically . writeTQueue eq $ (et :=> (Identity ())))
    return $ GHC.unregisterTimeout tm tk
  return e

-- | Provides a Dynamic UTCTime signal at the desired resolution.
--   
--   Slight drift in the form of more time then the request difference between timesteps is expected.
time :: (MonadALON t m)
     => DiffTime -> m (Dynamic t UTCTime)
time dt = do
  eq <- askEQ
  e <- newEventWithTrigger $ \et -> do
    t <- forkIO . forever $ do
           getCurrentTime >>= (atomically . writeTQueue eq . (et :=>) . Identity)
           -- This drifts. Not considered a problem for its intended use.
           threadDelay . floor $ dt*(10^(6::Int))
    return $ killThread t
  now <- liftIO getCurrentTime
  holdDyn now e

type DirTree a = TrieMap Text a

-- | Our basic ALON type for, representing at turns directories and site URL heirarchies.
--   In the future, we might make a special version of this type like 'Euhoria''s
--   <http://hackage.haskell.org/package/euphoria-0.6.0.1/docs/FRP-Euphoria-Collection.html Collection>
--   for more efficient updating.
type DynDirTree t a = Dynamic t (DirTree (Dynamic t a))


dirSource :: (Reflex t, MonadALON t m)
          => FP.FilePath -> m (DynDirTree t ByteString)
dirSource dir = do
    de <- watchDir dir
    let des = fanMap $ (uncurry Map.singleton) <$> de
        flEvent = select des . Const2 
        doDyn fl di = foldDyn (\e v ->
                                  case e of
                                    DataMod d -> d
                                    _ -> v) di . flEvent $ fl
        doDirTree c t =
            case c of
               (fp, DataDel) -> return . LT.delete fp $ t
               (fp, DataMod _) | LT.member fp t -> return t -- It'll update its self.
               (fp, DataMod d) | otherwise -> (\v -> LT.insert fp v t) <$> doDyn fp d
    -- Place de has to be active by.
    initS <- liftIO readDb
    initDir <- foldM (flip doDirTree) mempty initS
    foldDynM doDirTree initDir de
  where
    readDb :: IO [([Text], DataUpdate)]
    readDb = do
      runResourceT $
        sourceDirectoryDeep True dir $= CL.mapM (liftIO . readAsUpdate "") $$ CL.consume
