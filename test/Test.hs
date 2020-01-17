{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import           AlON.Manipulation
import           AlON.Source
import           AlON.Transform
import           Control.Monad
import           Control.Monad.Fix
import           Data.Bifunctor
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Time.Format.ISO8601
import qualified Data.Yaml as YAML
import           Reflex
import           Reflex.Filesystem.Internal
import           Reflex.Test
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TestVector

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "AlON Tests" $
    [ parseTimeGroup
    , testSelfTest
    , manipulationTests
    , transformTests
    ]

parseTimeGroup :: TestTree
parseTimeGroup =
  testGroup "parseGateTime Tests" . mconcat $
    [ map (\t -> testCase ("parse " ++ show t) (timeNonMidnight @=? parseGateTime t)) $
        [ "2016-03-18T05:27:04"
        , "2016-03-18T05:27:04Z"
        , "2016-03-18T05:27:04z"
        , "2016-03-18T05:27:04+00:00"
        , "2016-03-18T09:27:04+04:00"
        , "2016-03-18T05:27:04+0000"
        , "2016-03-18T09:27:04+0400"
        , "2016-03-18T05:27:04-00:00"
        , "2016-03-18T01:27:04-04:00"
        , "2016-03-18T05:27:04-0000"
        , "2016-03-18T01:27:04-0400"
        , "2016-03-18T05:27:04UTC"
        , "2016-03-18T00:27:04EST"
        , "2016-W11-5T05:27:04"
        , "2016-W11-5T05:27:04Z"
        , "2016-W11-5T05:27:04+00:00"
        , "2016-W11-5T05:27:04+0000"
        , "2016-W11-5T05:27:04-00:00"
        , "2016-W11-5T05:27:04-0000"
        , "2016-W11-5T05:27:04UTC"
        ]
 , map (\t -> testCase ("parse " ++ show t) (timeLateDay @=? parseGateTime t)) $
        [ "2016-03-18T18:27:04"
        , "2016-03-18T18:27:04Z"
        , "2016-03-18T18:27:04z"
        , "2016-03-18T18:27:04+0000"
        , "2016-03-18T18:27:04-0000"
        , "2016-03-18T18:27:04UTC"
        , "2016-W11-5T18:27:04"
        , "2016-W11-5T18:27:04Z"
        , "2016-W11-5T18:27:04+00:00"
        , "2016-W11-5T18:27:04+0000"
        , "2016-W11-5T18:27:04-00:00"
        , "2016-W11-5T18:27:04-0000"
        , "2016-W11-5T18:27:04UTC"
        ]
    , map (\t -> testCase ("parse " ++ show t) (timeMidnight @=? parseGateTime t)) $
        [ "2016-03-18"
        , "2016-W11-5"
        , "2016-03-18UTC"
        , "2016-03-18+0000"
        , "2016-W11-5+0000"
        , "2016-W11-5UTC"
        ]
    ]
  where
    timeLateDay, timeNonMidnight, timeMidnight :: Maybe UTCTime
    timeLateDay = Just $ read "2016-03-18 18:27:04Z"
    timeNonMidnight = Just $ read "2016-03-18 05:27:04Z"
    timeMidnight = Just $ read "2016-03-18 00:00:00Z"

testAlONErrorCase :: (Eq b, Show b)
                  => TestName
                  -> (forall t m . (Reflex t, Monad (Dynamic t), MonadHold t m, MonadFix m, DynamicWriter t [T.Text] m)
                           => Event t a -> m (Dynamic t b))
                  -> (b, [T.Text]) -- ^ Initial output value expected.
                  -> [(Maybe a, Maybe (b, [T.Text]), (b, [T.Text]))]
                  -- ^ Potential update, potential event output, expected output value.
                  -> TestTree
testAlONErrorCase tnm frm initVal cgen =
  testCase tnm $ do
    eventTrace cgen initVal $ \e -> do
      (o, errD) <- runDynamicWriterT $ frm e
      pure $ (,) <$> o <*> errD

testAlONCase :: (Eq b, Show b)
             => TestName
             -> (forall t m . (Reflex t, Monad (Dynamic t), MonadHold t m, MonadFix m)
                      => Event t a -> m (Dynamic t b))
             -> b -- ^ Initial output value expected.
             -> [(Maybe a, Maybe b, b)] -- ^ Potential update, potential event output, expected output value.
             -> TestTree
testAlONCase tnm frm initVal cgen =
    testAlONErrorCase tnm frm (augNoErrors initVal) (map (\(me, mo, o) -> (me, fmap augNoErrors mo, augNoErrors o)) cgen)
  where
    augNoErrors :: b -> (b, [T.Text])
    augNoErrors = (, [])

testSelfTest :: TestTree
testSelfTest =
  testGroup "Test Self Test"
    [ testAlONCase "test testAlONCase empty" (const . return . constDyn $ 'a') 'a' []
    , testAlONCase "test testAlONCase Nothing" (const . return . constDyn $ 'a') 'a' [(Nothing, Nothing, 'a')]
    , testAlONCase "test testAlONCase hold" (holdDyn 'a') 'a'
        [ (Nothing, Nothing, 'a')
        , (Just 'b', Just 'b', 'b')
        , (Just 'c', Just 'c', 'c')
        , (Just 'a', Just 'a', 'a')
        ]
    ]

sampleDirTree :: Reflex t => DynDirTree t a -> Dynamic t (DirTree a)
sampleDirTree = join . fmap sequenceA 

manipulationTests :: TestTree
manipulationTests =
  testGroup "Manipulation Tests" $
    let aTree        = LT.fromList [(["a", "b"], ())]::DirTree ()
        testEntry    = (["Test"], mempty::())
        testTree     = LT.fromList [testEntry]
        anotherEntry = (["Another"], mempty::())
        anotherTree  = LT.fromList [anotherEntry]
    in
    [ testAlONCase "merge mempty doesn't change a/b."
        (const . pure . sampleDirTree $ mergeDynTree (constDynDirTree aTree) (constDynDirTree mempty))
        aTree
        []
    , testAlONCase "merge nempty doesn't change b/a."
        (const . pure . sampleDirTree $ mergeDynTree (constDynDirTree mempty) (constDynDirTree aTree))
        aTree
        []
    , testAlONCase "merge nempty updates."
        (\e -> sampleDirTree <$> (mergeDynTree (constDynDirTree mempty) <$> (followDir [] e)))
        mempty
        [ (Just $ [fmap DataMod testEntry], Just testTree, testTree)
        , (Nothing, Nothing, testTree)
        , (Just $ [fmap DataMod anotherEntry], Just (testTree<>anotherTree), testTree<>anotherTree)
        , (Just $ [(fst anotherEntry, PathDel)], Just testTree, testTree)
        ]
    , testAlONCase "foldlDynDynList can sun"
        (\e -> do
          let (baseVE, listUpE) = fanEither e
          baseDyn <- holdDyn 0 baseVE
          listDyn <- foldDyn (\upE curList ->
            case upE of
              Left v -> constDyn v:curList
              Right i -> fmap (+i) (head curList):tail curList
            ) [constDyn 1, constDyn 2] listUpE
          pure $ foldlDynDynList (+) baseDyn listDyn)
        (3::Int)
        [ (Just (Left 1), Just 4, 4)
        , (Just (Right (Left 5)), Just 9, 9)
        , (Just (Right (Right 1)), Just 10, 10)
        ]
    , testAlONCase "apply2DynDirTree applies"
        (\e -> sampleDirTree . apply2DynDirTree show <$> (followDir [] e))
        mempty
        [ (Just [fmap DataMod testEntry], Just $ fmap show testTree, fmap show testTree)
        , (Nothing, Nothing, fmap show testTree)
        , (Just [fmap DataMod anotherEntry], Just $ fmap show (testTree<>anotherTree), fmap show $ testTree<>anotherTree)
        , (Just [(fst anotherEntry, PathDel)], Just $ fmap show testTree, fmap show testTree)
        ]
    , testAlONCase "mapDynTreeWithKey"
        (\e -> sampleDirTree . mapDynTreeWithKey (\t v -> TE.decodeUtf8 v:t) <$> followDir [(["init"], DataMod $ TE.encodeUtf8 "2")] e)
        (LT.fromList [(["init"], ["2", "init"])])
        [ ( Just [(["test"], (DataMod $ TE.encodeUtf8 "1"))]
          , Just $ LT.fromList [(["init"], ["2", "init"]), (["test"], ["1", "test"])], LT.fromList [(["init"], ["2", "init"]), (["test"], ["1", "test"])])
        , (Just $ [(["init"], PathDel)], Just $ LT.fromList [(["test"], ["1", "test"])], LT.fromList [(["test"], ["1", "test"])])
        ]
    ]

transformTests :: TestTree
transformTests =
  testGroup "Transform Tests" $
    [ let startTime = posixSecondsToUTCTime 1000
          asTree = LT.fromList . fmap (fmap newData)
          asTimePath = map (T.pack . iso8601Show)
          asMod = DataMod . TE.encodeUtf8
          toUpdate = map (bimap asTimePath asMod)
          laterUp = toUpdate [([1500   `addUTCTime` startTime], "afterer")]
          initialDir = toUpdate
            [ ([(-1000) `addUTCTime` startTime], "past")
            , ([                     startTime], "at")
            , ([ 1000   `addUTCTime` startTime], "after")
            ]
      in
      testAlONCase "timeGatedDir"
      (\e -> do
        let (timeE, dirE) = fanEither e
        timeBitsDyn <- utc2TimeBits <$> holdDyn startTime timeE
        dirDyn <- followDir initialDir dirE
        sampleDirTree <$> timeGatedDir timeBitsDyn dirDyn)
      (asTree . take 2 $ initialDir)
      [ (Just (Left $  500 `addUTCTime` startTime), Just (asTree . take 2 $ initialDir), (asTree . take 2 $ initialDir))
      , (Just (Left $ 1000 `addUTCTime` startTime), Just (asTree . take 3 $ initialDir), (asTree . take 3 $ initialDir))
        -- This should really be 3 showing it doesn't roll back.
      , (Just (Left $  999 `addUTCTime` startTime), Just (asTree . take 2 $ initialDir), (asTree . take 2 $ initialDir))
      , (Just (Left $ 1000 `addUTCTime` startTime), Just (asTree . take 3 $ initialDir), (asTree . take 3 $ initialDir))
      , (Just (Right $ laterUp), Just (asTree . take 3 $ initialDir), (asTree . take 3 $ initialDir))
      , (Just (Left $ 1600 `addUTCTime` startTime), Just (asTree $ initialDir <> laterUp), (asTree $ initialDir <> laterUp))
      ]
    , let mapUtf8 = fmap (fmap (DataMod . TE.encodeUtf8))
          initialDir = [([""], "")]
          extraGood  =  [(["new"], "test")]
      in
      testAlONCase "Decode UTF8 Tree"
      (\e ->  do
        d <- followDir (mapUtf8 initialDir) e
        pure $ sampleDirTree $ utf8DecodeDirTree d)
      (LT.fromList initialDir)
      [ (Just $ mapUtf8 extraGood, Just (LT.fromList $ initialDir <> extraGood), (LT.fromList $ initialDir <> extraGood))
      ]
    , let initialDir = [(["template.mustache"], DataMod "This is my {{ template }} for testing")]
      in
      testAlONErrorCase "cacheTemplates basic"
      (\e -> do
          fmap show <$> (cacheTemplates =<< followDir initialDir e))
      ("fromList [(\"template.mustache\",Template {name = \"template.mustache\", ast = [TextBlock \"This is my \",Variable True (NamedData [\"template\"]),TextBlock \" for testing\"], partials = fromList []})]", []) []
    , let initialDir = [(["template.mustache"], DataMod "This is my {{ template } for testing")
                       ,(["sub", "template.mustache"], DataMod "{{")
                       ]
      in
      testAlONErrorCase "cacheTemplates error"
      (\e -> do
          fmap show <$> (cacheTemplates =<< followDir initialDir e))
      ( "fromList []"
      , ["template.mustache : \"template.mustache\" (line 1, column 24) - \" \"\n\"}\"\n\"}\"\n\"}}\"\n\".\"\n"
        ,"sub/template.mustache : \"sub/template.mustache\" (line 1, column 3) - \n\n\n\n\n\n\n\n\n\n\n\n\n\n\"#\"\n\"/\"\n\"&\"\n\"{\"\n\">\"\n\"=\"\n\"^\"\n\"!\"\nwhite space\n\".\"\nwhite space\n\"}}\"\n\".\"\n"]) []
    , let initialDir = [ (["base.mustache"], DataMod "<h2>Names</h2>\n{{#names}}\n{{> user.mustache}}\n{{/names}}")
                       , (["user.mustache"], DataMod "<strong>{{name}}</strong>")
                       ]
      in
      testAlONErrorCase "cacheTemplates partial"
      (\e -> do
          fmap show <$> (cacheTemplates =<< followDir initialDir e))
      ("fromList [(\"user.mustache\",Template {name = \"user.mustache\", ast = [TextBlock \"<strong>\",Variable True (NamedData [\"name\"]),TextBlock \"</strong>\"], partials = fromList []}),(\"base.mustache\",Template {name = \"base.mustache\", ast = [TextBlock \"<h2>Names</h2>\\n\",Section (NamedData [\"names\"]) [Partial (Just \"\") \"user.mustache\"]], partials = fromList [(\"user.mustache\",Template {name = \"user.mustache\", ast = [TextBlock \"<strong>\",Variable True (NamedData [\"name\"]),TextBlock \"</strong>\"], partials = fromList []})]})]", []) []
    , let initialDir = [ (["base.mustache"], DataMod "<h2>Names</h2>\n{{#names}}\n{{> user.mustache}}\n{{/names}}")
                       , (["user.mustache"], DataMod "<strong>{{name}}</strong>")
                       ]
      in
      testAlONErrorCase "cacheTemplates render"
      (\e -> do
          let (de, ve) = fanEither e
          tc <- cacheTemplates =<< followDir initialDir de
          v <- (fmap $ Map.singleton ("names"::String) . fmap (Map.singleton ("name"::String))) <$> holdDyn [] ve
          render "base.mustache" tc v)
      ("<h2>Names</h2>\n", [])
      [ ( Just $ Right ["John"::String]
        , Just ("<h2>Names</h2>\n<strong>John</strong>", [])
        , ("<h2>Names</h2>\n<strong>John</strong>", []))
      , ( Just $ Left [(["user.mustache"], DataMod "{{name}}")]
        , Just ("<h2>Names</h2>\nJohn", [])
        , ("<h2>Names</h2>\nJohn", []))
      , ( Just $ Left [(["user.mustache"], PathDel)]
        , Just ("<h2>Names</h2>\n",["PartialNotFound \"user.mustache\""])
        , ("<h2>Names</h2>\n",["PartialNotFound \"user.mustache\""]))
      , ( Just $ Left [(["base.mustache"], PathDel)]
        , Just ("",["Couldn't find template base.mustache"])
        , ("",["Couldn't find template base.mustache"]))
      ]
    , testVectors "parseYamlHeaded" (\a b -> b == parseYamlHeaded a)
      [ ("file data", (Nothing, "file data"))
      , ("---\ntest: val\n---this is the file\nmore"
        , (Just (YAML.object [("test", YAML.String "val")]), "this is the file\nmore"))
      , ("---\ntest: val\r---this is the file\nmore"
        , (Just (YAML.object [("test", YAML.String "val")]), "this is the file\nmore"))
      , ("---\ntest: val\r\n---this is the file\nmore"
        , (Just (YAML.object [("test", YAML.String "val")]), "this is the file\nmore"))
      , ("---\ntest: val"
        , (Just (YAML.object [("test", YAML.String "val")]), ""))
      , ("---\n\n---\nthis is the file\nmore"
        , (Just YAML.Null, "\nthis is the file\nmore"))
      ]
    ]
