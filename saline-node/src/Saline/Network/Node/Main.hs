{-# LANGUAGE OverloadedStrings #-}
module Saline.Network.Node.Main where

import Control.Concurrent.Async (waitEither)
import Data.List.NonEmpty
import Data.Text
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Obelisk.Route
import System.Environment

import Saline.Interface.Query
import Saline.Network.CometBFT.CLI
import Saline.Network.Node.Handlers (runABCIExe)
import Saline.Network.Node.Testnet qualified as Testnet

main :: IO ()
main = do
  let unexpected = putStrLn "Usage: saline-node WATCHER_KEY_FILE NODE_DIR [-- COMETBFT_NODE_ARG...]"

  getArgs >>= \case
    (watcherKey: nodeDir: xs) -> let app = main' watcherKey nodeDir in case xs of
      [] -> app Nothing
      ("--": ys) -> app $ nonEmpty $ fmap Text.pack ys
      _ -> unexpected
    _ -> unexpected

main' :: FilePath -> FilePath -> Maybe (NonEmpty Text) -> IO ()
main' watcherKeyPath configPath passthrough = do
  enc <- case checkEncoder salineQueryEncoder of
    Left err -> error $ Text.unpack $ "main: " <> err
    Right enc -> pure enc

  n <- loadCometBFTNode $ Text.pack configPath

  posixTime <- fmap realToFrac getPOSIXTime

  --TODO: persistent app-level store

  let currentStore = Testnet.genesis posixTime

  (comet, saline) <- runNode passthrough (runABCIExe enc currentStore watcherKeyPath) n
  waitEither comet saline >>= \case
    Left _ -> error "cometbft async ended prematurely?"
    Right _ -> error "saline async ended prematurely?"
