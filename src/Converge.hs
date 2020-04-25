{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Converge where

import Converge.Api (Api, gitHubKey, server)
import Data.String.Interpolate (i)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Context ((:.)))
import qualified Servant
import Prelude hiding (id)


main :: IO ()
main = api


api :: MonadIO m => m ()
api = do
  let host = "localhost" :: Text
  let port = 7777
  let secret = "super-secret-code"

  putTextLn [i|Running at http://#{host}:#{port}|]
  liftIO $ Warp.run port
    (Servant.serveWithContext
      (Proxy @Api)
      (gitHubKey (pure secret) :. Servant.EmptyContext)
      server)
