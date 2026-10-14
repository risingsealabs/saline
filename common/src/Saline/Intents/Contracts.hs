module Saline.Intents.Contracts where

{-
import Control.Monad.Trans.Writer
import Data.Foldable
import qualified Data.Set as Set
import Data.Semigroup
import Data.String.Here
import Data.Text

import Saline.Intents.Condition

data Language
  = Language_Rust
  | Language_Solidity

data Target lang where
  Target_Ethereum :: Target 'Language_Solidity
  Target_Solana   :: Target 'Language_Rust

generateSupport :: Condition replicate Expr -> Target lang -> Text
generateSupport p t = gen $ execWriter $ collect p
  where
    gen xs = fold $ ($ t) <$> support xs
    support xs = header : condition : eval : flip fmap (Set.toList xs) (\(Arg _ g) -> g)
    add (k :: Text) = tell . Set.singleton . Arg k
    collect = \case
      Condition_Funds _  -> add "transfer" evalSig
      Condition_All ps   -> add "all" evalAll *> for_ ps collect
      Condition_Any _ ps -> add "any" evalAny *> for_ ps collect

header, condition, eval, evalSig, evalAll, evalAny :: Target lang -> Text
header = \case
  Target_Ethereum -> [i|
    // SPDX-License-Identifier: GPL-3.0
    pragma solidity ^0.8.8;
  |]
  Target_Solana -> [i|
    use borsh::{
      BorshDeserialize,
      BorshSerialize,
    };
  |]

condition = \case
  Target_Ethereum -> [i|
    enum Tag { Signature, All, Any }

    struct Condition {
      Tag tag;

      bytes32 signature;
      uint64 threshold:
      subpolicies Condition[];
    }
  |]

  Target_Solana -> [i|
    #[derive(BorshSerialize, BorshDeserialize, Debug)]
    enum Condition {
      Signature(Address)
      All([Condition])
      Any(u64, [Condition])
    }
    type Address = [u8:32]
  |]

eval = \case
  Target_Ethereum -> [i|
    function eval(Condition calldata p, bytes32[] calldata sigs) returns (bool) {
      return
        p.tag == Tag.Signature ? evalSig(p.signature, sigs)   :
        p.tag == Tag.All       ? evalAll(p.subpolicies, sigs) :
                                 evalAny(p.threshold, p.subpolicies, sigs)
    }
  |]
  Target_Solana -> [i|
    fn eval(p: Condition, sigs: &[Address]) -> bool {
      match p {
        Condition::Signature s => evalSig(s, sigs),
        Condition::All(ps)     => evalAll(ps, sigs),
        Condition::Any(k, ps)  => evalAny(k, ps, sigs),
      }
    }
  |]

evalAll = \case
  Target_Ethereum -> [i|
    function evalAll(Condition[] calldata subpolicies, bytes32[] calldata sigs) returns (bool) {
      for (uint i = 0; i < subpolicies.length ; i++) {
        if !eval(subpolicies[i], sigs) { return false; }
      }
      return true;
    }
  |]
  Target_Solana -> [i|
    fn evalAll(subpolicies: [Condition], sigs: &[Address]) -> bool {
      subpolicies.iter().all(|&p| eval(p, sigs))
    }
  |]

evalAny = \case
  Target_Ethereum -> [i|
    function evalAny(uint64 memory threshold, Condition[] calldata subpolicies, bytes32[] calldata sigs) {
      for (uint i = 0; i < subpolicies.length ; i++) {
        if threshold == 0 { return true; }
        if (eval(subpolicies[i], sigs)) { threshold--; }
      }
      return threshold == 0;
    }
  |]
  Target_Solana -> [i|
    fn evalAny(mut threshold: u64, subpolicies: [Condition], sigs: &[Address]) -> bool {
      for p in subpolicies.iter() {
        if threshold == 0 { return true; }
        if (eval(subpolicies[i], sigs)) { threshold--; }
      }
      return true;
    }
  |]

evalSig = \case
  Target_Ethereum -> [i|
    function evalSig(bytes32 memory sig, bytes32[] calldata sigs) returns (bool) {
      for (uint i = 0; i < sigs.length ; i++) {
        if (sig == sigs[i]) return true;
      }
      return false;
    }
  |]
  Target_Solana -> [i|
    fn evalSig(sig: [u8:32], sigs: &[Address]) -> bool {
      for s in sigs.iter() {
        if (sig == s) return true;
      }
      return false;
    }
  |]
-}
