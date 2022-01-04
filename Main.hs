{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
module Main where

import Control.Monad.Trans
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import Data.Text ()
--import Data.Time
import Network.Wai.Handler.Warp (defaultSettings)

import AlON.Types
import AlON.Source
import AlON.Manipulation
import AlON.Run
import AlON.WebServer
import AlON.ContentType.StaticFile

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
      eq <- askEQ

      --et <- time eq 1
      --let tbs = utc2TimeBits et
      --now <- liftIO $ getCurrentTime
      --tg <- afterTime tbs (5 `addUTCTime` now)
      dt <- apply2DynDirTree AnyContent . staticize . apply2DynDirTree BL.fromStrict <$> dirSource eq "test_dir"
      --sample (current dt) >>= mapM (sample . current) >>= liftIO . print
      --mt' <- dirSource eq "math_dir"
      --let mt = (\tgb d -> if tgb then d else mempty) <$> tg <*> mt'
      --let pt = mt -- mapDynTreeWithKey (\_ ds -> snd . runProcess $ (RunExternal "dc" [] ds)) mt
      return dt -- $ mergeDynTree pt dt


