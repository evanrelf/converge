{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Output.Extra (runOutputBoundedChan) where

import Control.Concurrent.Classy (MonadConc)
import Control.Concurrent.Classy.BoundedChan (BoundedChan, writeBoundedChan)
import Polysemy
import Polysemy.Output (Output (..))


runOutputBoundedChan
  :: MonadConc m
  => Member (Embed m) r
  => BoundedChan m o
  -> Sem (Output o ': r) a
  -> Sem r a
runOutputBoundedChan chan =
  interpret \(Output o) -> embed (writeBoundedChan chan o)
