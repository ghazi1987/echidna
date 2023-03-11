module Echidna.Types.Coverage where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Set (Set, size, map)

import Echidna.Types.Tx (TxResult)
import Data.Vector.Mutable (IOVector)

-- Program Counter directly obtained from the EVM
type PC = Int
-- Index per operation in the source code, obtained from the source mapping
type OpIx = Int
-- Stack size from the EVM
type FrameCount = Int
-- Basic coverage information
type CoverageInfo = (PC, OpIx, FrameCount, TxResult)
-- Map with the coverage information needed for fuzzing and source code printing
-- type CoverageMap = Map ByteString (Set CoverageInfo)
type CoverageMap = Map ByteString (IOVector (Int, Int, TxResult))

-- | Given good point coverage, count unique points.
coveragePoints :: CoverageMap -> Int
coveragePoints x = 0 -- sum . fmap size
-- | Given good point coverage, count the number of unique points but
-- only considering the different instruction PCs (discarding the TxResult).
-- This is useful to report a coverage measure to the user
scoveragePoints :: CoverageMap -> Int
scoveragePoints x = 0 -- sum . fmap (size . Data.Set.map (view _1))
