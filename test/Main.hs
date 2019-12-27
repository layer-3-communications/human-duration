{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

import Control.Applicative (liftA2)
import Data.Int (Int64)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.QuickCheck ((===))

import qualified Arithmetic.Nat as Nat
import qualified Data.Bytes as Bytes
import qualified Data.Duration.Human as D
import qualified Data.ByteArray.Builder.Bounded as BD
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ QC.testProperty "encode-decode-isomorphism" $ QC.forAll arbitraryDuration
      (\d ->
        D.decodeUtf8 (Bytes.fromByteArray (BD.run Nat.constant (D.boundedBuilderUtf8 d)))
        ===
        Just d
      )
  ]

arbitraryDuration :: QC.Gen D.Duration
arbitraryDuration = liftA2 D.Duration
  ( QC.elements
    [ D.Nanoseconds, D.Microseconds, D.Milliseconds
    , D.Seconds, D.Minutes, D.Hours, D.Days, D.Weeks
    ]
  )
  (fmap QC.getNonNegative (QC.arbitrary @(QC.NonNegative Int64)))
