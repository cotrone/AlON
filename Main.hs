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
import ALON.Transforms

import Reflex

main :: IO ()
main =
    runWarp defaultSettings frm
  where
    frm :: AlONSite
    frm = do
      et <- time 1
      dt <- dirSource "test_dir"
      mt <- dirSource "math_dir"
      pt <- mapDynTreeWithKey (\_ ds -> ((RunExternal "dc" []) <$> (sample . current $ ds)) >>= (fmap snd . runProcess)) mt
      rt <- mergeDynTree pt dt
      return rt
