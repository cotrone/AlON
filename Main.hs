{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import qualified Control.Exception as E
import Data.Bits
import Data.Text ()
import Data.Time
import Data.Time.Clock.POSIX
import Network.Wai.Handler.Warp (defaultSettings)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base (Assertion, (@=?))

import Reflex.Host.Class
import Data.Functor.Identity
import Data.Dependent.Map (DSum((:=>)))
import Control.Monad.Ref

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
      let mt = (\tgb d -> if tgb then d else mempty) <$> tg <*> mt'
      let pt = mapDynTreeWithKey (\_ ds -> snd . runProcess $ (RunExternal "dc" [] ds)) mt
      rt <- mergeDynTree pt dt
      return rt

