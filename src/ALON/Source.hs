{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module ALON.Source (
    TimeBits, utc2TimeBits, afterTime, afterTimeSpec, atTime, time
  , DirTree, DynDirTree, DataUpdate(..)
  , dirSource
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ListTrie.Patricia.Map.Ord (TrieMap)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified System.FilePath as FP
import qualified System.FSNotify as FSN
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($=), ($$))
import qualified Data.Conduit.List as CL
import Data.Conduit.Combinators (sourceDirectoryDeep)
import qualified Control.Exception as E
import Data.Functor.Misc
import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time
import System.Directory
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity
import qualified GHC.Event as GHC
import Data.Bits
import Data.Time.Clock.POSIX

import ALON.Types

-- | TimeBits is a list, each step containing the value of the time since the POSIX epoc,
--   shifted right one more then the last.
--   By using this, one can wait on the time changing more then a desired picosecond*2^Nat amount.
--   To get a precise wait, one may then have to wait again on a finer time counter.
--   The head of the list stores the full time as picoseconds since the unix epoch.
type TimeBits t = (Dynamic t UTCTime, [Dynamic t Integer])

-- | Create a (lazy) list of Dynamics, each holding a the value of the UTCTime right-shifted as many bits
--   as there are elements before it in the list.
utc2TimeBits :: forall t. (Reflex t, Functor (Dynamic t)) => Dynamic t UTCTime -> TimeBits t
utc2TimeBits dt = (dt, [timeSlice b <$> dt | b <- [0..]])

-- | Get the value of the UTCTime right-shifted the specified number of bits.
--   The conversion to Integer could be shared but would prevent code reuse.
timeSlice :: Int -> UTCTime -> Integer
timeSlice b = (`shiftR` b) . floor . (* 10^(12::Int)) . utcTimeToPOSIXSeconds

afterTimeSpec :: (Reflex t, MonadHold t m, MonadFix m, Functor (Dynamic t))
              => TimeBits t -> UTCTime -> m (Dynamic t Bool)
afterTimeSpec (curTime, _) tgtTime = do
  n <- sample . current $ curTime
  case tgtTime <= n of
    True -> return . constDyn $ True
    False ->
      holdDyn False =<< (onceE . ffilter (==True) . updated $ (((<) tgtTime) <$> curTime))

-- | Efficiently compare a target time with a timebits, becoming True at the chosen time.
--   This means that, until right before the the target time, almost all events are skipped.
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
        -- The switch causes a one-step delay on firing!
        holdDyn False =<< (onceE . switch . pull . allGreater $ prefix)
  where
    -- Check that all times are now greater than the target time
    allGreater :: [(Int, Dynamic t Integer)] -> PullM t (Event t Bool)
    allGreater [] = return . fmap (const True) . updated . head $ tbs
    -- When we get to the current time, we return always True rapidly.
    allGreater ((s, a):t) = do
      c <- sample . current $ a
      if c >= timeSlice s tgt
        then allGreater t
        else return never
    -- Find the prefix of time bits to a time >=
    dissimilarPrefix :: (Reflex t, MonadSample t m1)
                     => Int -> [(Int, Dynamic t Integer)] -> [Dynamic t Integer] -> m1 [(Int, Dynamic t Integer)]
    dissimilarPrefix _ _ [] = error "Unpossible, its an infinite list!"
    dissimilarPrefix step pre (cur:rest) = do
      thisStep <- sample . current $ cur
      if thisStep >= (timeSlice step tgt)
        then return pre
        else dissimilarPrefix (step+1) ((step, cur):pre) rest

-- | Efficiently fires an event at the target time, or immediately after.
--   Fires instantly if the time is already past.
--   
--   Does not take into account leap seconds.
atTime :: (Reflex t, MonadALON t m)
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
time :: (Reflex t, MonadALON t m)
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

data DataUpdate =
    DataMod ByteString
  | DataDel
  deriving (Eq, Ord, Show)

dirSource :: (Reflex t, MonadALON t m)
          => FP.FilePath -> m (DynDirTree t ByteString)
dirSource dir = do
    eq <- askEQ
    de <- newEventWithTrigger $ \et -> do
      t <- forkIO . FSN.withManagerConf (FSN.defaultConfig {FSN.confUsePolling = False}) $ \m -> do
        E.handle (\(e::E.SomeException) -> putStrLn ("Excp: "++show e)) $ do
          wq <- newTQueueIO
          -- Start listenin before we read the dir
          void . FSN.watchTree m dir (const True) $ atomically . writeTQueue wq
          -- Then we just watch the changes, and send them on
          -- Having stripped off the leading path
          pwd <- getCurrentDirectory
          forever . E.handle (\(e::E.SomeException) -> putStrLn ("Excp: "++show e)) $ do
            e <- atomically (readTQueue wq) >>= e2e pwd
            atomically . writeTQueue eq $ et :=> (Identity e)
      return (killThread t)
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
    e2e :: FP.FilePath -> FSN.Event -> IO ([Text], DataUpdate)
    e2e pf (FSN.Added fp _) = r pf fp
    e2e pf (FSN.Modified fp _) = r pf fp
    e2e pf (FSN.Removed fp _) =
        return $ ( map T.pack . drop 1 . FP.splitDirectories . FP.makeRelative pf $ fp
                 , DataDel)
    r :: FP.FilePath -> FP.FilePath -> IO ([Text], DataUpdate)
    r pf fp = do
      d <- liftIO . BS.readFile $ fp
      d `deepseq` return ( map T.pack . drop 1 . FP.splitDirectories . FP.makeRelative pf $ fp
                         , DataMod d)
    readDb :: IO [([Text], DataUpdate)]
    readDb = do
      runResourceT $
        sourceDirectoryDeep True dir $= CL.mapM (liftIO . r "") $$ CL.consume
