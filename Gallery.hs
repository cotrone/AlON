{-# LANGUAGE BlockArguments, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
module Main where

import Control.Monad
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified Data.Text as T
import qualified Data.Text.IO as TI
--import Data.Time
import Network.Wai.Handler.Warp (defaultSettings)
import qualified Text.Blaze.Html5 as HTML5
import qualified Text.Blaze.Html5.Attributes as HTML5A

import AlON.Types
import AlON.Source
import AlON.Manipulation
import AlON.Run
import AlON.WebServer
import AlON.ContentType.StaticFile
import AlON.ContentType.HTML

import Reflex
import AlON.Static

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False err = E.throwIO . E.AssertionFailed $ err

main :: IO ()
main = do
    staticizeSite (TI.putStrLn . T.intercalate "\n") "gallery.tar" frm
    runWarp defaultSettings frm
  where
    frm :: AlONSite
    frm = do
      eq <- askEQ

      --et <- time eq 1
      --let tbs = utc2TimeBits et
      --now <- liftIO $ getCurrentTime
      --tg <- afterTime tbs (5 `addUTCTime` now)
      dt <- apply2DynDirTree AnyContent . staticize . apply2DynDirTree BL.fromStrict <$> dirSource eq "gallery"
      let gp = do
            contentTree <- dt
            pure . AnyContent . htmlize $ do
              HTML5.docTypeHtml $ do
                HTML5.head $ pure ()
                HTML5.body $ do
                  HTML5.ul . forM_ (fst <$> LT.toList contentTree) $ \imgPath -> do
                    HTML5.li $ do
                      HTML5.img HTML5.! HTML5A.src (HTML5.toValue $ "/" <> T.intercalate "/" imgPath)
      let ft = mergeDynTree dt (constDyn $ LT.singleton ["index.html"] gp)
      --sample (current ft) >>= mapM (sample . current) >>= liftIO . print
      --mt' <- dirSource eq "math_dir"
      --let mt = (\tgb d -> if tgb then d else mempty) <$> tg <*> mt'
      --let pt = mt -- mapDynTreeWithKey (\_ ds -> snd . runProcess $ (RunExternal "dc" [] ds)) mt
      pure ft --  $ mergeDynTree pt dt
