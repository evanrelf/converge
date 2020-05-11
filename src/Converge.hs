module Converge where

import qualified Converge.Api as Api
import Effect.Log (Verbosity (..), logToIO)
import Polysemy
import Polysemy.AtomicState (atomicStateToState)
import Polysemy.IO (embedToMonadIO)
import Polysemy.State (evalState)


main :: IO ()
main = do
  Api.run 7777 "super-secret-code"
    ( runM
    . embedToMonadIO
    . evalState mempty
    . atomicStateToState
    . logToIO Vomit
    )
