{-# LANGUAGE OverloadedStrings #-}
module Saline.Dev where

import Control.Exception
import Control.Monad
import Data.Text.IO qualified as Text
import Data.Word (Word16)
import Network.Wai.Handler.Warp (defaultSettings, setBeforeMainLoop, setPort, setTimeout)
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets.Connection (defaultConnectionOptions)
import Obelisk.Backend
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Run (obeliskApp, run, defaultRunApp, runServeAsset)
import System.Directory
import System.Exit
import Text.URI qualified as URI

import Saline.App
import Saline.Backend
import Saline.Crypto (Password)
import Saline.Interface.Route
import Saline.Network
import Saline.Utils
import Saline.Test qualified

-- Public development-only password, retained for the local demo workflow.
-- Never reuse it for a real wallet.
autoUnlock :: Maybe Password
autoUnlock = Just "abAB12!\""

dev :: FilePath -> IO ()
dev rootDir = do
  catch Saline.Test.dev $ \e -> when (e == ExitSuccess) $
    withDevnet False True $ runAppDev rootDir backend $ Frontend frontendHead (frontendBody False autoUnlock)

runAppDev :: FilePath -> Backend backendRoute frontendRoute -> Frontend (R frontendRoute) -> IO ()
runAppDev rootDir b f = do
  let
    configsParent = rootDir
    assetsParent = rootDir

  withCurrentDirectory configsParent $
    run $ defaultRunApp b f $ runServeAsset $ assetsParent <> "/static"

-- Very stripped down version of Obelisk.Run.runWidget
-- TODO: upstream or so?
type Port = Word16
runWidgetFrontendOnly :: Port -> AppEncoder -> Frontend (R AppRoute) -> IO ()
runWidgetFrontendOnly port enc front = do
  uri <- URI.mkURI $ "http://localhost:" <> tshow port
  let
    settings = setBeforeMainLoop beforeMainLoop (setPort (fromEnum port) (setTimeout 3600 defaultSettings))
    beforeMainLoop = Text.putStrLn $ "Running on " <> URI.render uri

  app <- obeliskApp mempty defaultConnectionOptions front enc uri $ \_ _ -> do
    --TODO: somehow require casing on `Void1` to prove this or factor it out of `obeliskApp`
    error "Should be impossible as this can't actually be reached when there's no backend routes"

  Warp.runSettings settings app
