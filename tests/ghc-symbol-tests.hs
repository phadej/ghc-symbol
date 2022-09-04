{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import Test.Tasty       (defaultMain, testGroup)
import Test.Tasty.HUnit

import Data.Proxy (Proxy (..))
import GHC.Symbol

main :: IO ()
main = defaultMain $ testGroup "ghc-symbol"
    [ testCase "Eq" $ do
        assertBool "same" $ symbolVal (Proxy @"foo") == symbolVal (Proxy @"foo")
        assertBool "not"  $ symbolVal (Proxy @"foo") /= symbolVal (Proxy @"bar")

    -- this is interesting :)
    , testCaseSteps "dangerous" $ \info -> do
        info $ dangerous "foo"

    ]

dangerous :: Symbol -> String
dangerous x = case x of
