{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.Wallet.PipeliningStrategy
  ( variablePipelining
  , pipeliningStrategyFromPredefined
  , PredefinedPipeliningStrategy (..)
  )
where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( NetworkParameters, blockHeight, genesisParameters, header )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock, StandardCrypto, fromCardanoBlock )
import Data.Quantity
    ( Quantity (Quantity) )
import Data.Word
    ( Word32 )
import Ouroboros.Network.Client.Wallet
    ( Pipelining (Pipelining), PipeliningStrategy )

-- | Pipelining strategies that have been in use

data PredefinedPipeliningStrategy 
    = ConstPipelining 
    | VariablePipelining 
    deriving (Show)

pipeliningsOfStrat
    :: PredefinedPipeliningStrategy
    -> Quantity "block" Word32
    -> Pipelining
pipeliningsOfStrat ConstPipelining _ = Pipelining 1000
pipeliningsOfStrat VariablePipelining (Quantity blockNo)
    | blockNo <= 5_200_000 = Pipelining 1000
    | blockNo <= 6_100_000 = Pipelining 200
    | blockNo <= 6_500_000 = Pipelining 125
    | otherwise            = Pipelining 100

variablePipelining
    :: NetworkParameters
    -> PipeliningStrategy (CardanoBlock StandardCrypto)
variablePipelining = pipeliningStrategyFromPredefined VariablePipelining

pipeliningStrategyFromPredefined
    :: PredefinedPipeliningStrategy
    -> NetworkParameters
    -> PipeliningStrategy (CardanoBlock StandardCrypto)
pipeliningStrategyFromPredefined strat gp =
    pipeliningsOfStrat strat
        . blockHeight
        . header
        . fromCardanoBlock (genesisParameters gp)
