{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.AtomicState.Extra (runAtomicStateTVar) where

import Control.Concurrent.Classy (MonadSTM)
import Control.Concurrent.Classy.STM.TVar (TVar, readTVar, writeTVar)
import Polysemy
import Polysemy.AtomicState (AtomicState (..))
import Prelude hiding (TVar, readTVar, writeTVar)


runAtomicStateTVar
  :: MonadSTM m
  => Member (Embed m) r
  => TVar m s
  -> Sem (AtomicState s ': r) a
  -> Sem r a
runAtomicStateTVar tvar = interpret \case
  AtomicState f -> embed do
    (s', x) <- f <$> readTVar tvar
    writeTVar tvar s'
    pure x
  AtomicGet -> embed (readTVar tvar)
