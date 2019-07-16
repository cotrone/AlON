{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import           AlON.Manipulation
import           AlON.Source
import           AlON.Transform
import           Control.Monad
import           Data.Bifunctor
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Time.Format.ISO8601
import           Reflex
import           Reflex.Filesystem.Internal
import           Reflex.Host.Class
import           Reflex.Test
import           Test.Tasty
import           Test.Tasty.HUnit

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
    timeLateDay = Just $ read "2016-03-18 18:27:04"
    timeNonMidnight = Just $ read "2016-03-18 05:27:04"
    timeMidnight = Just $ read "2016-03-18 00:00:00"

testAlONCase :: (Eq b, Show b)
             => TestName
             -> (forall t. (ReflexHost t) => Event t a -> HostFrame t (Dynamic t b))
             -> b -- ^ Initial output value expected.
             -> [(Maybe a, Maybe b, b)] -- ^ Potential update, potential event output, expected output value.
             -> TestTree
testAlONCase tnm frm initVal cgen = testCase tnm $ eventTrace cgen initVal frm

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
        testEntry    = (["Test"], mempty)
        testTree     = LT.fromList [testEntry]
        anotherEntry = (["Another"], mempty)
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
        [ (Just $ uncurry Map.singleton (fmap DataMod testEntry), Just testTree, testTree)
        , (Nothing, Nothing, testTree)
        , (Just $ uncurry Map.singleton (fmap DataMod anotherEntry), Just (testTree<>anotherTree), testTree<>anotherTree)
        , (Just $ Map.singleton (fst anotherEntry) PathDel, Just testTree, testTree)
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
        [ (Just $ uncurry Map.singleton (fmap DataMod testEntry), Just $ fmap show testTree, fmap show testTree)
        , (Nothing, Nothing, fmap show testTree)
        , (Just $ uncurry Map.singleton (fmap DataMod anotherEntry), Just $ fmap show (testTree<>anotherTree), fmap show $ testTree<>anotherTree)
        , (Just $ Map.singleton (fst anotherEntry) PathDel, Just $ fmap show testTree, fmap show testTree)
        ]
    , testAlONCase "mapDynTreeWithKey"
        (\e -> sampleDirTree . mapDynTreeWithKey (\t v -> TE.decodeUtf8 v:t) <$> followDir [(["init"], DataMod $ TE.encodeUtf8 "2")] e)
        (LT.fromList [(["init"], ["2", "init"])])
        [ ( Just $ Map.singleton ["test"] (DataMod $ TE.encodeUtf8 "1")
          , Just $ LT.fromList [(["init"], ["2", "init"]), (["test"], ["1", "test"])], LT.fromList [(["init"], ["2", "init"]), (["test"], ["1", "test"])])
        , (Just $ Map.singleton ["init"] PathDel, Just $ LT.fromList [(["test"], ["1", "test"])], LT.fromList [(["test"], ["1", "test"])])
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
      , (Just (Right $ Map.fromList $ laterUp), Just (asTree . take 3 $ initialDir), (asTree . take 3 $ initialDir))
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
      [ (Just $ Map.fromList $ mapUtf8 extraGood, Just (LT.fromList $ initialDir <> extraGood), (LT.fromList $ initialDir <> extraGood))
      ]
    ]
