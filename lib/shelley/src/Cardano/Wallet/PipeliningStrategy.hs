{-# LANGUAGE NumericUnderscores #-}

module Cardano.Wallet.PipeliningStrategy where

import Cardano.Wallet.Primitive.Types
    ( blockHeight, header, NetworkParameters, genesisParameters )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock, StandardCrypto, fromCardanoBlock )
import Data.List
    ( find )
import Data.Quantity
    ( Quantity (Quantity) )
import Numeric.Natural
    ( Natural )
import Ouroboros.Network.Client.Wallet
    ( Pipelining (Pipelining), PipeliningStrategy )
import Prelude

data PipeliningStrat = ConstPipelining | VariablePipelining deriving (Show)

pipeliningsOfStrat
    :: Num y
    => PipeliningStrat
    -> ([(Quantity x y, Natural)], Natural)
pipeliningsOfStrat ConstPipelining = ([], 1000)
pipeliningsOfStrat VariablePipelining =
    ( [ (Quantity 4_000_000, 1000)
        , (Quantity 5_000_000, 200)
        , (Quantity 6_000_000, 100)
        ]
    , 50
    )

pipeliningsOfHeight
    :: (Ord y, Num y)
    => PipeliningStrat
    -> Quantity x y
    -> Pipelining
pipeliningsOfHeight strat tip =
    Pipelining $
        maybe bed snd (find (\(q, _) -> q >= tip) pipelinings)
    where
        (pipelinings, bed) = pipeliningsOfStrat strat

variablePipelining
    :: NetworkParameters
    -> PipeliningStrategy (CardanoBlock StandardCrypto)
variablePipelining = stratPipelining VariablePipelining

stratPipelining 
    :: PipeliningStrat
    -> NetworkParameters 
    -> PipeliningStrategy (CardanoBlock StandardCrypto)
stratPipelining strat gp =
    pipeliningsOfHeight strat
        . blockHeight
        . header
        . fromCardanoBlock (genesisParameters gp)
