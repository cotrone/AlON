{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import ALON.Source
import ALON.Run

main :: IO ()
main =
    runSite print (mapM_ print) frm
  where
    frm eq = do
      --et <- time eq 1
      dt <- dirSource eq "test_dir"
      return dt
