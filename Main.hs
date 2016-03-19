{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import Control.Monad.Trans
import qualified Control.Exception as E
import Data.Text ()
import Data.Time
import Network.Wai.Handler.Warp (defaultSettings)
import ALON.Source
import ALON.Manipulation
import ALON.Run
import ALON.WebServer
import ALON.Transforms

import Reflex

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False err = E.throwIO . E.AssertionFailed $ err

main :: IO ()
main = do
    assert (all ((==) (Just $ read "2016-03-18 05:27:04")) . map parseGateTime $
                      [ "2016-03-18T05:27:04"
                      , "2016-03-18T05:27:04Z"
                      , "2016-03-18T05:27:04+00:00"
                      , "2016-03-18T09:27:04+04:00"
                      , "2016-W11-5T05:27:04"
                      , "2016-W11-5T05:27:04Z"
                      , "2016-W11-5T05:27:04+00:00" ]) $
      "Non-midnight times failed to parse equal."
    assert (all ((==) (Just $ read "2016-03-18 00:00:00")) . map parseGateTime $ [ "2016-03-18", "2016-W11-5" ]) $
      "Midnight times failed to parse equal."
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
