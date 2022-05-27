module Cardano.Wallet.Primitive.Types.Coin.Gen
    ( chooseCoin
    , genCoin
    , genCoinPositive
    , shrinkCoin
    , shrinkCoinPositive
    , partitionCoin
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Control.Monad
    ( replicateM )
import Data.List.NonEmpty
    ( NonEmpty )
import Test.QuickCheck
    ( Gen, choose, sized )
import Test.QuickCheck.Extra
    ( chooseNatural, shrinkNatural )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Choosing coins from a range.
--------------------------------------------------------------------------------

chooseCoin :: (Coin, Coin) -> Gen Coin
chooseCoin (Coin a, Coin b) =
    Coin <$> chooseNatural (a, b)

--------------------------------------------------------------------------------
-- Coins chosen according to the size parameter.
--------------------------------------------------------------------------------

genCoin :: Gen Coin
genCoin = sized $ \n -> Coin . fromIntegral <$> choose (0, n)

shrinkCoin :: Coin -> [Coin]
shrinkCoin (Coin c) = Coin <$> shrinkNatural c

--------------------------------------------------------------------------------
-- Coins chosen according to the size parameter, but strictly positive.
--------------------------------------------------------------------------------

genCoinPositive :: Gen Coin
genCoinPositive = sized $ \n -> Coin . fromIntegral <$> choose (1, max 1 n)

shrinkCoinPositive :: Coin -> [Coin]
shrinkCoinPositive (Coin c) = Coin <$> filter (> 0) (shrinkNatural c)

--------------------------------------------------------------------------------
-- Partitioning coins
--------------------------------------------------------------------------------

-- | Partitions a coin randomly into a given number of parts.
--
-- Satisfies the following properties:
--
-- prop> forAll (partitionCoin c i) $ (==       c) . fold
-- prop> forAll (partitionCoin c i) $ (== max 1 i) . length
--
partitionCoin :: Coin -> Int -> Gen (NonEmpty Coin)
partitionCoin c i =
    Coin.partitionDefault c <$> genWeights
  where
    genWeights :: Gen (NonEmpty Coin)
    genWeights = NE.fromList <$> replicateM (max 1 i)
        (chooseCoin (Coin 1, max (Coin 1) c))
