{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai.Handler.Warp (defaultSettings)
import ALON.Source
import ALON.Manipulation
import ALON.Run
import ALON.Types
import ALON.WebServer

import Reflex

main :: IO ()
main =
    runWarp defaultSettings frm
  where
    frm :: AlONSite
    frm = do
      alonLogErrors (constDyn ["Test error"])
      et <- time 1
      dt <- dirSource "test_dir"
      mt <- dirSource "math_dir"
      -- fmap (RunExternal "dc" []) <$> 
      rt <- mergeDynTree mt dt 
      return rt
