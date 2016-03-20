{-# LANGUAGE ScopedTypeVariables #-}
module ALON.Source.Internal (
    DataUpdate(..), watchDir, readAsUpdate
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.FilePath as FP
import qualified System.FSNotify as FSN
import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Control.Concurrent.STM
import System.Directory
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity

import ALON.Types

data DataUpdate =
    DataMod ByteString
  | DataDel
  deriving (Eq, Ord, Show)

readAsUpdate :: FP.FilePath -> FP.FilePath -> IO ([Text], DataUpdate)
readAsUpdate pf fp = do
  d <- liftIO . BS.readFile $ fp
  d `deepseq` return ( map T.pack . drop 1 . FP.splitDirectories . FP.makeRelative pf $ fp
                         , DataMod d)

watchDir :: (Reflex t, MonadALON t m) => FilePath -> m (Event t ([Text], DataUpdate))
watchDir dir = do
    eq <- askEQ
    newEventWithTrigger $ \et -> do
      fsnm <- FSN.startManagerConf (FSN.defaultConfig {FSN.confUsePolling = False})
      -- Get our base path so we can strip off the leading path.
      pwd <- getCurrentDirectory
      -- Start listenin before we read the dir.
      void . FSN.watchTree fsnm dir (const True) $ \upEvent -> do
        e <- e2e pwd upEvent
        atomically . writeTQueue eq $ et :=> (Identity e)
      return (FSN.stopManager fsnm)
  where
    e2e :: FP.FilePath -> FSN.Event -> IO ([Text], DataUpdate)
    e2e pf (FSN.Added fp _) = readAsUpdate pf fp
    e2e pf (FSN.Modified fp _) = readAsUpdate pf fp
    e2e pf (FSN.Removed fp _) =
        return $ ( map T.pack . drop 1 . FP.splitDirectories . FP.makeRelative pf $ fp
                 , DataDel)
