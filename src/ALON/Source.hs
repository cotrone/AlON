{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module ALON.Source (
    time
  , DirTree, DataUpdate(..)
  , dirSource
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Control.Concurrent.Chan
import Control.Monad.Loops
import Data.ListTrie.Patricia.Map.Ord (TrieMap)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified Filesystem as FS
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FP
import qualified System.FSNotify as FSN
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($=), ($$))
import qualified Data.Conduit.List as CL
import Data.Conduit.Combinators (sourceDirectoryDeep)
import qualified Control.Exception as E
import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Data.Time
import qualified Data.IVar.Simple as IVar

time :: (MonadIO m, MonadHold Spider m, MonadReflexCreateTrigger Spider m) => TQueue (DSum (EventTrigger Spider)) -> DiffTime -> m (Dynamic Spider UTCTime)
time q dt = do
  e <- newEventWithTrigger $ \et -> do
    t <- forkIO . forever $ do
           getCurrentTime >>= (atomically . writeTQueue q . (et :=>))
           -- This drifts. Not considered a problem for its intended use.
           threadDelay . floor $ dt*(10^6)
    return $ killThread t
  now <- liftIO getCurrentTime
  holdDyn now e

type DirTree a = TrieMap FP.FilePath a

data DataUpdate =
    DataMod [FP.FilePath] ByteString
  | DataDel [FP.FilePath]
  deriving (Eq, Ord, Show)

dirSource :: (MonadIO m, MonadHold Spider m, MonadReflexCreateTrigger Spider m) => TQueue (DSum (EventTrigger Spider)) -> FP.FilePath -> m (Dynamic Spider (DirTree (Dynamic Spider ByteString)))
dirSource q dir = do
    initS::IVar.IVar [DataUpdate] <- liftIO IVar.new 
    de <- newEventWithTrigger $ \et -> do
      t <- forkIO . FSN.withManagerConf (FSN.defaultConfig {FSN.confUsePolling = False}) $ \m ->
        E.handle (\(e::E.SomeException) -> putStrLn ("Excp: "++show e)) $ do
          eq <- newTQueueIO
          -- Start listening for changes to the directory, feeding it to us via STM due to issues of isEmptyChan.
          FSN.watchTree m dir (const True) (atomically . writeTQueue eq)
          -- Get an initial read of the directory, and issues with read correctness will be covered by inotify
          ifs <- readDb
          IVar.write initS ifs
          pre <- atomically . whileM (not <$> isEmptyTQueue eq) $ readTQueue eq
          -- We have to convert the stable read to update events, overlay the changes as events, and emit a big event.
          pres <- mapM e2e pre
          atomically . writeTQueue q $ et :=> pres
          -- Then we just watch the changes, and send them on
          forever . E.handle (\(e::E.SomeException) -> putStrLn ("Excp: "++show e)) $ do
            e <- atomically (readTQueue eq) >>= e2e
            atomically . writeTQueue q $ et :=> [e]
      return (killThread t)
    let dirCont = IVar.read initS
    
    return de
  where
    e2e :: FSN.Event -> IO DataUpdate
    e2e (FSN.Added fp _) = r fp
    e2e (FSN.Modified fp _) = r fp
    e2e (FSN.Removed fp _) = return $ DataDel (FP.splitDirectories fp)
    r :: FP.FilePath -> IO DataUpdate
    r fp = do
      d <- liftIO . FS.readFile $ fp
      d `deepseq` return (DataMod (FP.splitDirectories fp) d)
    readDb :: IO [DataUpdate]
    readDb = do
      runResourceT $
        sourceDirectoryDeep True (FP.encodeString dir) $= CL.mapM (liftIO . r . FP.decodeString) $$ CL.consume
