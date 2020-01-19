{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import Servant (Context((:.)))

import qualified Converge

main :: IO ()
main = do
  let host = "localhost"
  let port = 8080
  let secret = "super-secret-code"
  putTextLn ("Running at http://" <> host <> ":" <> show port)
  Warp.run port
    (Servant.serveWithContext
      (Proxy @Converge.WebhookApi)
      (Converge.gitHubKey (pure secret) :. Servant.EmptyContext)
      Converge.server)
