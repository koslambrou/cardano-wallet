{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.CoinSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, partitionCoin, shrinkCoin, shrinkCoinPositive )
import Data.Function
    ( (&) )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Testable
    , checkCoverage
    , conjoin
    , cover
    , forAll
    , property
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.Foldable as F

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Types.CoinSpec" $ do

    parallel $ describe "Arithmetic operations" $ do

        it "prop_add_toNatural" $ do
            property prop_add_toNatural
        it "prop_add_subtract" $ do
            property prop_add_subtract
        it "prop_difference_distance" $ do
            property prop_difference_distance
        it "prop_difference_subtract" $ do
            property prop_difference_subtract
        it "prop_distance_commutative" $ do
            property prop_distance_commutative
        it "prop_subtract_toNatural" $ do
            property prop_subtract_toNatural

    parallel $ describe "Generators and shrinkers" $ do

        describe "Coins that can be zero" $ do
            it "genCoin_coverage" $
                property prop_genCoin_coverage
            it "shrinkCoin" $
                property prop_shrinkCoin

        describe "Coins that are strictly positive" $ do
            it "genCoinPositive" $
                property prop_genCoinPositive
            it "shrinkCoinPositive" $
                property prop_shrinkCoinPositive

    parallel $ describe "Generating partitions" $ do

        it "prop_partitionCoin_fold" $
            prop_partitionCoin_fold & property
        it "prop_partitionCoin_length" $
            prop_partitionCoin_length & property

--------------------------------------------------------------------------------
-- Arithmetic operations
--------------------------------------------------------------------------------

prop_add_subtract :: Coin -> Coin -> Property
prop_add_subtract a b =
    checkCoverageCoin a b $
    conjoin
    [ (a `Coin.add` b) `Coin.subtract` b === Just a
    , (b `Coin.add` a) `Coin.subtract` a === Just b
    ]

prop_add_toNatural :: Coin -> Coin -> Property
prop_add_toNatural a b =
    checkCoverageCoin a b $
    (===)
        (Coin.toNatural (a `Coin.add` b))
        (Coin.toNatural a + Coin.toNatural b)

prop_difference_distance :: Coin -> Coin -> Property
prop_difference_distance a b =
    checkCoverageCoin a b $
    if (a >= b)
    then a `Coin.distance` b == a `Coin.difference` b
    else a `Coin.distance` b == b `Coin.difference` a

prop_difference_subtract :: Coin -> Coin -> Property
prop_difference_subtract a b =
    checkCoverageCoin a b $
    if (a >= b)
    then a `Coin.subtract` b === Just (a `Coin.difference` b)
    else a `Coin.subtract` b === Nothing

prop_distance_commutative :: Coin -> Coin -> Property
prop_distance_commutative a b =
    checkCoverageCoin a b $
    a `Coin.distance` b === b `Coin.distance` a

prop_subtract_toNatural :: Coin -> Coin -> Property
prop_subtract_toNatural a b =
    checkCoverageCoin a b $
    if (a >= b)
    then
        (Coin.toNatural <$> (a `Coin.subtract` b))
        ===
        (Just (Coin.toNatural a - Coin.toNatural b))
    else
        (Coin.toNatural <$> (b `Coin.subtract` a))
        ===
        (Just (Coin.toNatural b - Coin.toNatural a))

--------------------------------------------------------------------------------
-- Coins that can be zero
--------------------------------------------------------------------------------

prop_genCoin_coverage :: Coin -> Coin -> Property
prop_genCoin_coverage a b =
    checkCoverageCoin a b True

checkCoverageCoin :: Testable prop => Coin -> Coin -> (prop -> Property)
checkCoverageCoin a b
    = checkCoverage
    . cover  1 (a == Coin 0 && b == Coin 0) "a == 0 && b == 0"
    . cover  2 (a == Coin 0 && b /= Coin 0) "a == 0 && b /= 0"
    . cover  2 (a /= Coin 0 && b == Coin 0) "a /= 0 && b == 0"
    . cover 10 (a /= Coin 0 && b /= Coin 0) "a /= 0 && b /= 0"
    . cover 10 (a <  b) "a < b"
    . cover  2 (a == b) "a = b"
    . cover 10 (a >  b) "a > b"

prop_shrinkCoin :: Property
prop_shrinkCoin = forAll genCoin $ \c ->
    let shrunken = shrinkCoin c in
    all (< c) shrunken

--------------------------------------------------------------------------------
-- Coins that are strictly positive
--------------------------------------------------------------------------------

prop_genCoinPositive :: Property
prop_genCoinPositive = forAll genCoinPositive isValidCoinPositive

prop_shrinkCoinPositive :: Property
prop_shrinkCoinPositive = forAll genCoinPositive $ \c ->
    let shrunken = shrinkCoinPositive c in
    conjoin $ ($ shrunken) <$>
        [ all (< c)
        , all isValidCoinPositive
        ]

isValidCoinPositive :: Coin -> Bool
isValidCoinPositive c = c > Coin 0

--------------------------------------------------------------------------------
-- Generating partitions
--------------------------------------------------------------------------------

prop_partitionCoin_fold :: Coin -> Int -> Property
prop_partitionCoin_fold c i =
    forAll (partitionCoin c i) $ (== c) . F.fold

prop_partitionCoin_length :: Coin -> Int -> Property
prop_partitionCoin_length c i =
    forAll (partitionCoin c i) $ (== max 1 i) . F.length

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Coin where
    arbitrary = genCoin
    shrink = shrinkCoin
