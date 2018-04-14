{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
module Main where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad.Trans
import qualified Control.Exception as E
import Data.Text ()
import Data.Time
import Network.Wai.Handler.Warp (defaultSettings)

import ALON.Types
import ALON.Source
import ALON.Manipulation
import ALON.Run
import ALON.WebServer

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False err = E.throwIO . E.AssertionFailed $ err

main :: IO ()
main = do
    runWarp defaultSettings frm
  where
    frm :: AlONSite
    frm = do
      -- This is a temporary fix while reflex-inotify is being split out.
      eq <- askEQ
      let q = atomically . mapM_ (writeTQueue eq)

      et <- time 1
      let tbs = utc2TimeBits et
      now <- liftIO $ getCurrentTime
      tg <- afterTime tbs (5 `addUTCTime` now)
      dt <- dirSource q "test_dir"
      mt' <- dirSource q "math_dir"
      let mt = (\tgb d -> if tgb then d else mempty) <$> tg <*> mt'
      let pt = mt -- mapDynTreeWithKey (\_ ds -> snd . runProcess $ (RunExternal "dc" [] ds)) mt
      return $ mergeDynTree pt dt


