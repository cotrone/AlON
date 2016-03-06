{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import Control.Monad.Trans
import Data.Time
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai.Handler.Warp (defaultSettings)
import ALON.Source
import ALON.Manipulation
import ALON.Run
import ALON.Types
import ALON.WebServer
import ALON.Transforms

import Reflex

main :: IO ()
main =
    runWarp defaultSettings frm
  where
    frm :: AlONSite
    frm = do
      et <- time 1
      let tbs = utc2TimeBits et
      now <- liftIO $ getCurrentTime
      tg <- afterTime tbs (5 `addUTCTime` now)
      dt <- dirSource "test_dir"
      mt' <- dirSource "math_dir"
      mt <- combineDyn (\tgb d -> if tgb then d else mempty) tg mt'
      let pt = mapDynTreeWithKey (\_ ds -> snd . runProcess $ (RunExternal "dc" [] ds)) mt
      rt <- mergeDynTree pt dt
      return rt
