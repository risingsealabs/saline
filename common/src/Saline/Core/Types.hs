{-# LANGUAGE TemplateHaskell #-}
module Saline.Core.Types where

import Control.Monad
import Control.Lens.TH
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC.Natural

import qualified Data.Set as Set
import Data.Text (Text)

-- * Widely used types

type Amount = Natural
type LocalAddr = String
type ProtocolVersion = ()
type Date = ()

-- * Addresses & Networks

type SalineAddr = Text
type EthAddr = String
type BtcAddr = String
type DataAddr = String

data RemoteAddr
  = RAEth EthAddr
  | RABtc BtcAddr
  deriving stock (Eq, Ord, Show)

type NetworkID = String

data Addr
  = Saline SalineAddr -- local:xxx
  | Remote RemoteAddr -- eth:yyy
  deriving stock (Eq, Ord, Show)

type Curr = String
type Assets = Map Curr Amount
data Currency addr = Currency
  { _curCode :: Curr
  , _curName :: String
  , _curAddr :: addr
  } deriving stock (Eq, Ord, Show)

data Network = Network
  { _netName :: String
  , _netID :: NetworkID
  , _netCurrencies :: Set (Currency RemoteAddr)
  } deriving stock Show

-- * Public & private keys, signatures

type PublicKey = Integer
type SecretKey = Integer

trustedWatcherPK :: PublicKey
trustedWatcherPK = 0

type Signers = Set PublicKey

isSignedBy :: PublicKey -> Signers -> Bool
isSignedBy pk keys = Set.member pk keys

pubKeyOwns :: PublicKey -> Addr -> Bool
pubKeyOwns _ _ = True

-- * Proofs

newtype WatcherProof = WP Signers
  deriving stock Show

verifyWatcherProof :: WatcherProof -> Bool
verifyWatcherProof (WP s) = isSignedBy trustedWatcherPK s

data ZKProof = ZKP
  deriving stock Show

verifyZK :: ZKProof -> Bool
verifyZK _ = True

data Proof
  = WatcherProof WatcherProof
  | ZKProof      ZKProof
  deriving stock Show

verifyProof :: Proof -> Bool
verifyProof p = case p of
  ZKProof zkp -> verifyZK zkp
  WatcherProof wp -> verifyWatcherProof wp

-- * Intents

-- data IBound a = IBound { boundVal :: a, boundIncluded :: Bool }
--   deriving (Eq, Show, Functor)

-- data Interval a = Interval (IBound a) (IBound a)
--   deriving (Eq, Show, Functor)

-- type Time = Natural

-- data Frequency = Hourly | Daily | Weekly | Monthly | Yearly
--   deriving (Eq, Show)

-- type Times = Natural

data TimeInfo = OneOff --  | Repeat Times | Recurring Frequency | TimeWindow (Interval Time)
  deriving stock (Eq, Show)

-- data TipInfo = NoTip --  | TipExactly Assets | TipBetween (...) ?
--   deriving (Eq, Show)

data Intent = Intent
  deriving stock (Eq, Show)

type Intents = [Intent]

-- * Transactions

type DataTx = ()
type CurrencyTx = ()

data TxShape a
  = Prim a       -- ^ just a simple, primitive transaction
  | Gate [a] [a] -- ^ upon receiving the inbound txs in the first list,
                 --   allow the outbound txs of the second list
  deriving stock Show

data SwapTx = SwapTx
  { _swapAddrA, _swapAddrB :: Addr
  , _swapWhatA, _swapWhatB :: Assets
  } deriving stock Show

data SimpleTx = SimpleTx
  { _stxFrom, _stxTo :: Addr
  , _stxWhat :: Assets
  , _stxData :: Maybe ByteString
  } deriving stock Show

data AssetsTx
  = TxSimple SimpleTx -- ^ simple one-way transfers of assets
  | TxSwap SwapTx     -- ^ swap
  deriving stock Show

data IntentTx
  = InstallIntent Addr Assets Intent (Maybe Proof)
  | RemoveIntent Addr Natural
  deriving stock Show

data LedgerTx
  = TxIntent IntentTx      -- ^ intent installation, removal etc
  | TxAssets (TxShape AssetsTx) -- ^ asset movement on Saline
  | TxData   DataTx        -- ^ on-chain data slot creation, update etc
  deriving stock Show

data GenesisTx = GenesisTx
  { _genesisVersion :: ProtocolVersion
  } deriving stock Show

data ProtocolTx
  = ProtocolTxUpgrade ProtocolVersion Date
  | ProtocolTxUpgradeNow ProtocolVersion
  deriving stock Show

data TxMeta = TxMeta
  { _metaSigners :: Signers
  , _metaProof   :: Maybe Proof
  , _metaExtra   :: Maybe ByteString
  } deriving stock Show

data Tx a = Tx
  { _txData :: a
  , _txMeta :: TxMeta
  } deriving stock Show

data ChainTx
  = TxGenesis  (Tx GenesisTx)  -- ^ hello world from the chain
  | TxProtocol (Tx ProtocolTx) -- ^ protocol upgrades etc
  | TxLedger   (Tx LedgerTx)   -- ^ alter distributed state maintained by the chain
  | TxCurrency (Tx CurrencyTx) -- ^ creating/delete Saline currencies/tokens or new derivatives on existing currencies
  | TxBundle   [ChainTx]       -- ^ ordered, atomic bundle of txs
  deriving stock Show

-- * On-chain data

type DataSlot = ()

-- * Chain state

data LedgerState = LedgerState
  { _ledgerAssets     :: Map Addr Assets
  , _ledgerIntents    :: Map Addr Intents
  , _ledgerCurrencies :: Set (Currency SalineAddr)
  , _ledgerData       :: Map DataAddr DataSlot
  } deriving stock Show

data ChainState = ChainState
  { _chainNetworks   :: Set Network
  , _chainLedger     :: LedgerState
  , _chainValidators :: Set PublicKey
  , _chainProto      :: ProtocolVersion
  , _chainNextProto  :: Maybe (ProtocolVersion, Date)
  } deriving stock Show

emptyLS :: LedgerState
emptyLS = LedgerState mempty mempty mempty mempty

-- * Blocks

type BlockHeader = ()

data Block = Block
  { _blockHeader :: BlockHeader
  , _blockTxs    :: [ChainTx]
  } deriving stock Show

-- examples

-- basic transfer
-- swap
-- 2 person lottery
-- install one-off intent A, one-off intent B, make tx to fulfill both
-- amm
-- ...

fmap join $ sequence
  [ makeLenses ''Block
  , makeLenses ''ChainState
  , makeLenses ''Currency
  , makeLenses ''GenesisTx
  , makeLenses ''LedgerState
  , makeLenses ''Network
  , makeLenses ''SimpleTx
  , makeLenses ''SwapTx
  , makeLenses ''Tx
  , makeLenses ''TxMeta
  , makePrisms ''Addr
  , makePrisms ''AssetsTx
  , makePrisms ''ChainTx
  , makePrisms ''IntentTx
  , makePrisms ''LedgerTx
  , makePrisms ''Proof
  , makePrisms ''ProtocolTx
  , makePrisms ''RemoteAddr
  , makePrisms ''TxShape
  ]
