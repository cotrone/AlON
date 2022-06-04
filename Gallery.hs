{-# LANGUAGE BlockArguments, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes, TypeFamilies #-}
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
import System.IO
import Control.Monad.IO.Class

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
  withFile "gallery.tar" ReadWriteMode $ \tarHandle -> do
    staticizeSite (TI.putStrLn . T.intercalate "\n") frm (writeToHandle tarHandle)
  putStrLn "Static site created"
  runWarp defaultSettings frm

frm :: AlONSite 
frm = do
  postBuild <- getPostBuild
  performEvent $ (liftIO $ putStrLn "build fired") <$ postBuild
  gFp <- holdDyn "gallery" ("gallery" <$ postBuild)
  dir <- dirSource gFp
  performEvent $ liftIO . print <$> dirUpdates  dir
  let dt = apply2DynDirTree AnyContent . staticize $ apply2DynDirTree BL.fromStrict dir
  let gp = do
        contentTree <- dt
        pure . AnyContent . htmlize $ do
          HTML5.docTypeHtml $ do
            HTML5.head $ pure ()
            HTML5.body $ do
              HTML5.ul . forM_ (fst <$> LT.toList contentTree) $ \imgPath -> do
                HTML5.li $ do
                  HTML5.img HTML5.! HTML5A.src (HTML5.toValue $ "/" <> T.intercalate "/" imgPath)
  pure $ mergeDynTree dt (constDyn $ LT.singleton [] gp)

dirUpdates :: Reflex t => DynDirTree t a -> Event t (DirTree a)
dirUpdates dynDir = updated $ do
  content <- dynDir
  fmap LT.fromList $ mapM (\(p, c) -> (,) p <$> c) $ LT.toList content