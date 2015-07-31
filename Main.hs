{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import Network.Wai.Handler.Warp (defaultSettings)
import ALON.Source
import ALON.WebServer

main :: IO ()
main =
    runWarp defaultSettings frm
  where
    frm eq = do
      --et <- time eq 1
      dt <- dirSource eq "test_dir"
      return dt
