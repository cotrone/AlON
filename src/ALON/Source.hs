{-# LANGUAGE ScopedTypeVariables #-}
module ALON.Source (
    DirTree
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
import Reflex.Spider.Internal (newEventWithTriggerIO)
import Data.Dependent.Sum (DSum ((:=>)))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

type DirTree a = TrieMap FP.FilePath a

data DataUpdate =
    DataMod [FP.FilePath] ByteString
  | DataDel [FP.FilePath]
  deriving (Eq, Ord, Show)

dirSource :: FP.FilePath -> IO (Event Spider [DataUpdate])
dirSource dir = do
  newEventWithTriggerIO $ \et -> do
    t <- forkIO . FSN.withManagerConf (FSN.defaultConfig {FSN.confUsePolling = False}) $ \m -> do
      eq <- newTQueueIO
      -- Start listening for changes to the directory, feeding it to us via STM due to issues of isEmptyChan.
      FSN.watchTree m dir (const True) (atomically . writeTQueue eq)
      -- Get an initial read of the directory, and issues with read correctness will be covered by inotify
      ifs <- liftIO readDb
      pre <- atomically . whileM (not <$> isEmptyTQueue eq) $ readTQueue eq
      -- We have to convert the stable read to update events, overlay the changes as events, and emit a big event.
      pres <- mapM e2e pre
      runSpiderHost $ fireEvents [et :=> pres]
      -- Then we just watch the changes, and send them on
      forever . E.handle (\(e::E.SomeException) -> print e) $ do
        e <- atomically (readTQueue eq) >>= e2e
        runSpiderHost $ fireEvents [et :=> [e]]
    return (killThread t)
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
