module Saline.Backend where

import Obelisk.Backend
import Obelisk.Route

import Saline.App
import Saline.Interface.Route

main :: IO ()
main = runBackend backend frontend

backend :: Backend Void1 AppRoute
backend = Backend
  { _backend_run = \serve -> serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
