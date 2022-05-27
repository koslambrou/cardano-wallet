module Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange
    , genTokenBundleSmallRangePositive
    , genTokenBundle
    , shrinkTokenBundle
    , shrinkTokenBundleSmallRange
    , shrinkTokenBundleSmallRangePositive
    , partitionTokenBundle
    , partitionTokenBundleNonNull
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, partitionCoin, shrinkCoin, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap, genTokenMapSmallRange, partitionTokenMap, shrinkTokenMap )
import Data.List.NonEmpty
    ( NonEmpty )
import Test.QuickCheck
    ( Gen )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Token bundles with variable numbers of assets, the upper bound being
-- QuickCheck's size parameter.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--------------------------------------------------------------------------------

genTokenBundle :: Gen TokenBundle
genTokenBundle = TokenBundle
    <$> genCoin
    <*> genTokenMap

shrinkTokenBundle :: TokenBundle -> [TokenBundle]
shrinkTokenBundle (TokenBundle c m)=
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoin)
        (m, shrinkTokenMap)

--------------------------------------------------------------------------------
-- Token bundles with coins, assets, and quantities chosen from small ranges
--------------------------------------------------------------------------------

genTokenBundleSmallRange :: Gen TokenBundle
genTokenBundleSmallRange = TokenBundle
    <$> genCoin
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRange :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRange = shrinkTokenBundle

genTokenBundleSmallRangePositive :: Gen TokenBundle
genTokenBundleSmallRangePositive = TokenBundle
    <$> genCoinPositive
    <*> genTokenMapSmallRange

shrinkTokenBundleSmallRangePositive :: TokenBundle -> [TokenBundle]
shrinkTokenBundleSmallRangePositive (TokenBundle c m) =
    uncurry TokenBundle <$> shrinkInterleaved
        (c, shrinkCoinPositive)
        (m, shrinkTokenMap)

--------------------------------------------------------------------------------
-- Partitioning token bundles
--------------------------------------------------------------------------------

-- | Partitions a token bundle randomly into a given number of parts.
--
-- Satisfies the following properties:
--
-- prop> forAll (partitionTokenBundle b i) $ (==       b) . fold
-- prop> forAll (partitionTokenBundle b i) $ (== max 1 i) . length
--
partitionTokenBundle :: TokenBundle -> Int -> Gen (NonEmpty TokenBundle)
partitionTokenBundle (TokenBundle c m) i =
    NE.zipWith TokenBundle
        <$> partitionCoin     c i
        <*> partitionTokenMap m i

partitionTokenBundleNonNull :: TokenBundle -> Int -> Gen [TokenBundle]
partitionTokenBundleNonNull m i =
    filter (/= mempty) . F.toList <$> partitionTokenBundle m i
