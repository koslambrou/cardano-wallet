{-# LANGUAGE RankNTypes, ScopedTypeVariables, OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- 'Store' implementations that can store various wallet types
-- in an SQLite database using `persistent`.
--
-- FIXME LATER during ADP-1043:
--
-- * Inline the contents of "Cardano.Wallet.DB.Sqlite.CheckpointsOld"
--   into this module
-- * Use 'Table' and 'Embedding' to construct the relevant 'Store'
--   rather than implementing 'loadS', 'writeS', 'updateS' in
--   a monadic fashion.
--   Hide the new implementation behind a feature flag,
--   i.e. "Cardano.Wallet.DB.Sqlite.StoresNew".

module Cardano.Wallet.DB.Sqlite.Stores
    ( mkStoreWallets
    , PersistAddressBook (..)
    , blockHeaderFromEntity
    -- * Testing
    , mkStoreWallet
    )
    where

import Cardano.Wallet.DB.Stores.Checkpoints.Sqlite
import Data.DBVar (Store (Store, loadS, writeS, updateS))
import Database.Persist.Sql (SqlPersistT, Entity, deleteWhere, (==.), entityVal, selectList)
import Data.DeltaMap (DeltaMap (Insert, Delete, Adjust))
import Prelude
import Cardano.Wallet.DB.WalletState (DeltaWalletState, DeltaWalletState1 (ReplacePrologue, UpdateCheckpoints, UpdateTransactions), WalletState (WalletState))
import Cardano.Wallet.DB.Sqlite.TH (Wallet, EntityField (CheckpointWalletId))
import Data.Generics.Internal.VL (view, (^.))
import Control.Monad.Except (runExceptT, ExceptT (ExceptT))
import Control.Monad (forM)
import Cardano.Wallet.DB.Stores.TxHistory.Sqlite (mkStoreTransactions)
import Control.Exception (toException)
import Cardano.Wallet.DB (ErrBadFormat(ErrBadFormatAddressPrologue))

import qualified Data.Map.Strict as Map
import qualified Cardano.Wallet.Primitive.Types as W

 {-------------------------------------------------------------------------------
    WalletState Store
-------------------------------------------------------------------------------}
-- | Store for 'WalletState' of multiple different wallets.
mkStoreWallets
    :: forall s key. (PersistAddressBook s, key ~ W.WalletId)
    => Store (SqlPersistT IO)
        (DeltaMap key (DeltaWalletState s))
mkStoreWallets = Store{loadS=load,writeS=write,updateS=update}
  where
    write = error "mkStoreWalletsCheckpoints: not implemented"

    update _ (Insert wid a) =
        writeS (mkStoreWallet wid) a
    update _ (Delete wid) = do
        -- FIXME LATER during ADP-1043:
        --  Deleting an entry in the Checkpoint table
        --  will trigger a delete cascade. We want this cascade
        --  to be explicit in our code.
        deleteWhere [CheckpointWalletId ==. wid]
    update _ (Adjust wid da) =
        updateS (mkStoreWallet wid) undefined da
        -- FIXME LATER during ADP-1043:
        --   Remove 'undefined'.
        --   Probably needs a change to 'Data.DBVar.updateS'
        --   to take a 'Maybe a' as parameter instead of an 'a'.

    load = do
        wids <- fmap (view #walId . entityVal) <$> selectAll
        runExceptT $ do
            xs <- forM wids $ ExceptT . loadS . mkStoreWallet
            pure $ Map.fromList (zip wids xs)
      where
        selectAll :: SqlPersistT IO [Entity Wallet]
        selectAll = selectList [] []

-- | Store for 'WalletState' of a single wallet.
mkStoreWallet
    :: forall s. PersistAddressBook s
    => W.WalletId
    -> Store (SqlPersistT IO) (DeltaWalletState s)
mkStoreWallet wid =
    Store{ loadS = load, writeS = write, updateS = \_ -> update }
  where
    storeCheckpoints = mkStoreCheckpoints wid
    storeTransactions = mkStoreTransactions wid

    load = do
        eprologue <- maybe
            (Left $ toException ErrBadFormatAddressPrologue) Right
                <$> loadPrologue wid
        echeckpoints <- loadS storeCheckpoints
        etransactions <- loadS storeTransactions
        pure $ WalletState <$> eprologue <*> echeckpoints <*> etransactions

    write wallet = do
        insertPrologue wid (wallet ^. #prologue)
        writeS storeCheckpoints (wallet ^. #checkpoints)

    update =
         -- first update in list is last to be applied!
        mapM_ update1 . reverse
    update1 (ReplacePrologue prologue) =
        insertPrologue wid prologue
    update1 (UpdateCheckpoints delta) =
        -- FIXME LATER during ADP-1043: remove 'undefined'
        updateS storeCheckpoints undefined delta
    update1 (UpdateTransactions delta) =
        -- FIXME LATER during ADP-1043: remove 'undefined'
        updateS storeTransactions undefined delta
