{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)

    -- * Helpers
    , waitForConnection
    , defaultRetryPolicy

    -- * Errors
    , ErrNetworkUnavailable (..)
    , ErrNetworkTip (..)
    , ErrGetBlock (..)
    , ErrPostTx (..)
    , ErrDecodeExternalTx (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), Tx, TxWitness )
import Control.Exception
    ( Exception (..), throwIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Control.Retry
    ( RetryPolicyM, constantDelay, limitRetriesByCumulativeDelay, retrying )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

data NetworkLayer t m = NetworkLayer
    { nextBlocks :: BlockHeader -> ExceptT ErrGetBlock m [Block (Tx t)]
        -- ^ Fetches a contiguous sequence of blocks from the node, starting
        -- from the first block available with a slot greater than the given
        -- block header.
        --
        -- Blocks are returned in ascending slot order, without skipping blocks.
        --
        -- This function will not necessarily return all blocks available after
        -- the given point in time, but will return a reasonably-sized sequence.
        --
        -- It may return the empty list if the node does not have any blocks
        -- after the specified starting slot.

    , networkTip
        :: ExceptT ErrNetworkTip m (BlockHeader, Quantity "block" Natural)
        -- ^ Get the current network tip from the chain producer and the current
        -- chain's height.

    , postTx
        :: (Tx t, [TxWitness]) -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer

    , decodeExternalTx
        :: ByteString -> ExceptT ErrDecodeExternalTx m (Tx t, [TxWitness])
        -- ^ Decode an externally-signed transaction to the chain producer
    }

-- | Network is unavailable
data ErrNetworkUnavailable
    = ErrNetworkUnreachable Text
      -- ^ Cannot connect to network backend.
    | ErrNetworkInvalid Text
      -- ^ Network backend reports that the requested network is invalid.
    deriving (Generic, Show, Eq)

-- | Exception predicate for 'ErrNetworkUnreachable'.
isNetworkUnreachable :: ErrNetworkUnavailable -> Bool
isNetworkUnreachable (ErrNetworkUnreachable _) = True
isNetworkUnreachable (ErrNetworkInvalid _) = False

-- | Error while trying to get the network tip
data ErrNetworkTip
    = ErrNetworkTipNetworkUnreachable ErrNetworkUnavailable
    | ErrNetworkTipNotFound
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkTip

-- | Error while trying to get one or more blocks
data ErrGetBlock
    = ErrGetBlockNetworkUnreachable ErrNetworkUnavailable
    | ErrGetBlockNotFound (Hash "BlockHeader")
    deriving (Show, Eq)

-- | Error while trying to send a transaction
data ErrPostTx
    = ErrPostTxNetworkUnreachable ErrNetworkUnavailable
    | ErrPostTxBadRequest Text
    | ErrPostTxProtocolFailure Text
    deriving (Generic, Show, Eq)

instance Exception ErrPostTx

-- | Error while trying to decode externally signed transaction
data ErrDecodeExternalTx
    = ErrDecodeExternalTxWrongPayload Text
    | ErrDecodeExternalTxNotSupported
    deriving (Generic, Show, Eq)

instance Exception ErrDecodeExternalTx

-- | Wait until 'networkTip networkLayer' succeeds according to a given
-- retry policy. Throws an exception otherwise.
waitForConnection
    :: NetworkLayer t IO
    -> RetryPolicyM IO
    -> IO ()
waitForConnection nw policy = do
    r <- retrying policy shouldRetry (const $ runExceptT (networkTip nw))
    case r of
        Right _ -> return ()
        Left e -> throwIO e
  where
    shouldRetry _ = \case
        Right _ ->
            return False
        Left ErrNetworkTipNotFound ->
            return True
        Left (ErrNetworkTipNetworkUnreachable e) ->
            return $ isNetworkUnreachable e

-- | A default 'RetryPolicy' with a constant delay, but retries for no longer
-- than a minute.
defaultRetryPolicy :: Monad m => RetryPolicyM m
defaultRetryPolicy =
    limitRetriesByCumulativeDelay (60 * second) (constantDelay (1 * second))
  where
    second = 1000*1000
