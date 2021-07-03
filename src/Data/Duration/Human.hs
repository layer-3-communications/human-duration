{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language TypeApplications #-}

module Data.Duration.Human
  ( -- * Types
    Duration(..)
  , Measure(..)
    -- * Conversion
  , toNanoseconds
  , toTimespan
    -- * Decode
  , decodeUtf8
    -- * Encode
  , boundedBuilderUtf8
  ) where

import Chronos (Timespan(Timespan))
import Data.Int (Int64)
import Data.Bytes.Parser (Parser)
import Data.Bytes (Bytes)

import qualified Arithmetic.Lte as Lte
import qualified Data.Bytes.Builder.Bounded as BD
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as Latin

data Duration = Duration !Measure !Int64
  deriving stock (Show,Eq)

data Measure
  = Nanoseconds
  | Microseconds
  | Milliseconds
  | Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
  deriving stock (Show,Eq)

toTimespan :: Duration -> Timespan
toTimespan = Timespan . toNanoseconds

toNanoseconds :: Duration -> Int64
toNanoseconds (Duration msr n) = multiplier msr * n

multiplier :: Measure -> Int64
multiplier = \case
  Nanoseconds -> 1
  Microseconds -> 1_000
  Milliseconds -> 1_000_000
  Seconds -> 1_000_000_000
  Minutes -> 60_000_000_000
  Hours -> 3_600_000_000_000
  Days -> 86_400_000_000_000
  Weeks -> 604_800_000_000_000

boundedBuilderUtf8 :: Duration -> BD.Builder 22
boundedBuilderUtf8 = builder_abbrev

builder_abbrev :: Duration -> BD.Builder 22
builder_abbrev (Duration msr n) =
  BD.int64Dec n
  `BD.append`
  builderMeasure_abbrev msr

builderMeasure_abbrev :: Measure -> BD.Builder 2
builderMeasure_abbrev = \case
  Nanoseconds -> BD.ascii 'n' `BD.append` BD.ascii 's'
  Microseconds -> BD.ascii 'u' `BD.append` BD.ascii 's'
  Milliseconds -> BD.ascii 'm' `BD.append` BD.ascii 's'
  Seconds -> BD.weaken Lte.constant (BD.ascii 's')
  Minutes -> BD.weaken Lte.constant (BD.ascii 'm')
  Hours -> BD.weaken Lte.constant (BD.ascii 'h')
  Days -> BD.weaken Lte.constant (BD.ascii 'd')
  Weeks -> BD.weaken Lte.constant (BD.ascii 'w')

decodeUtf8 :: Bytes -> Maybe Duration
decodeUtf8 = P.parseBytesMaybe (parserUtf8 ())

parserUtf8 :: e -> Parser e s Duration
parserUtf8 e = do
  n' <- Latin.decUnsignedInt e
  let !n = fromIntegral @Int @Int64 n'
  Latin.skipChar ' '
  Latin.any e >>= \case
    'h' -> Latin.opt >>= \case
      Nothing -> pure $! Duration Hours n
      Just 'o' -> do
        Latin.char3 e 'u' 'r' 's'
        pure $! Duration Hours n
      Just _ -> P.fail e
    's' -> Latin.opt >>= \case
      Nothing -> pure $! Duration Seconds n
      Just 'e' -> do
        Latin.char5 e 'c' 'o' 'n' 'd' 's'
        pure $! Duration Seconds n
      Just _ -> P.fail e
    'u' -> do
      Latin.char e 's'
      pure $! Duration Microseconds n
    'w' -> Latin.opt >>= \case
      Nothing -> pure $! Duration Weeks n
      Just 'e' -> do
        Latin.char3 e 'e' 'k' 's'
        pure $! Duration Weeks n
      Just _ -> P.fail e
    'n' -> Latin.any e >>= \case
      's' -> pure $! Duration Nanoseconds n
      'a' -> do
        Latin.char9 e 'n' 'o' 's' 'e' 'c' 'o' 'n' 'd' 's'
        pure $! Duration Nanoseconds n
      _ -> P.fail e
    'm' -> Latin.opt >>= \case
      Nothing -> pure $! Duration Minutes n
      Just 's' -> pure $! Duration Milliseconds n
      Just 'i' -> Latin.any e >>= \case
        'c' -> do
          Latin.char9 e 'r' 'o' 's' 'e' 'c' 'o' 'n' 'd' 's'
          pure $! Duration Microseconds n
        'l' -> do
          Latin.char9 e 'l' 'i' 's' 'e' 'c' 'o' 'n' 'd' 's'
          pure $! Duration Milliseconds n
        'n' -> do
          Latin.char4 e 'u' 't' 'e' 's'
          pure $! Duration Minutes n
        _ -> P.fail e
      Just _ -> P.fail e
    'd' -> Latin.opt >>= \case
      Nothing -> pure $! Duration Days n
      Just 'a' -> do
        Latin.char2 e 'y' 's'
        pure $! Duration Days n
      Just _ -> P.fail e
