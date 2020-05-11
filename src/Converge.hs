module Converge where

import qualified Converge.Api as Api
import Effect.Log (Verbosity (..), logToIO)
import Polysemy


main :: IO ()
main = do
  Api.run 7777 "super-secret-code" (runM . logToIO Vomit)
