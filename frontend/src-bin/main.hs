{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import Obelisk.Frontend (runFrontend)
import Obelisk.Route (checkEncoder)
import Reflex.Dom (run)

import Saline.App (frontend)
import Saline.Interface.Route (fullRouteEncoder)

main :: IO ()
main = case checkEncoder fullRouteEncoder of
  Left err -> error $ Text.unpack $ "main: " <> err
  Right validFullEncoder -> run $ runFrontend validFullEncoder frontend
