module Converge (main) where

import Control.Concurrent.STM.TVar (newTVar)
import qualified Converge.Api as Api
import Effect.Log (Verbosity (..), logToIO)
import Polysemy (runM)
import Polysemy.AtomicState (runAtomicStateTVar)
import Polysemy.IO (embedToMonadIO)


main :: IO ()
main = do
  worldTVar <- atomically (newTVar mempty)

  Api.run 7777 "super-secret-code"
    ( runM
    . embedToMonadIO
    . runAtomicStateTVar worldTVar
    . logToIO Vomit
    )
