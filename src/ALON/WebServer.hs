module ALON.WebServer (
  ) where

import qualified Network.Wai.Handler.Warp as Warp

import ALON.Run

data SiteStruct =
  SS { sContent :: DirTree ByteString
     , ssEvents :: DirTree (TChan ())
     }

runWarp :: Warp.Settings -> AlONSite -> IO ()
runWarp settings site = do
  siteState <- newTVarIO (SS mempty mempty)
  let upSite ups = atomically $ do
      i' <- readTVar siteState
      (\a -> foldM_ a i' ups) $ \ i (fp, md) -> do
         case md of
           Nothing -> do
                   maybe (return ()) (flip writeTChan ()) . LT.lookup fp . ssEvents $ i
                   return $ SS (LT.delete fp . sContent $ i) (LT.delete fp . ssEvents $ i)
  runSite (putMVar siteState) upSite site

