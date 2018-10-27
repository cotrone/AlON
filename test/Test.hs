{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Data.Text ()

import Test.Tasty
import Test.Tasty.HUnit

import Reflex.Host.Class
import Reflex.Test

import AlON.Transforms

import Reflex

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "AlON Tests" $
    [ parseTimeGroup
    , testSelfTest
    , alonUpdateTests
    ]

parseTimeGroup :: TestTree
parseTimeGroup =
  testGroup "parseGateTime tests" . mconcat $
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
             -> b -> [(Maybe a, Maybe b, b)]
             -> TestTree
testAlONCase tnm frm initVal cgen = testCase tnm $ eventTrace cgen initVal frm

testSelfTest :: TestTree
testSelfTest =
  testGroup "test self test" $
    [ testAlONCase "test testAlONCase empty" (const . return . constDyn $ 'a') 'a' []
    , testAlONCase "test testAlONCase Nothing" (const . return . constDyn $ 'a') 'a' [(Nothing, Nothing, 'a')]
    , testAlONCase "test testAlONCase hold" (holdDyn 'a') 'a'
        [ (Nothing, Nothing, 'a')
        , (Just 'b', Just 'b', 'b')
        , (Just 'c', Just 'c', 'c')
        , (Just 'a', Just 'a', 'a')
        ]
    ]

alonUpdateTests :: TestTree
alonUpdateTests =
    testGroup "AlON update tests" $
      [
      ]
