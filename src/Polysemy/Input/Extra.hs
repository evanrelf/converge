{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Input.Extra (runInputBoundedChan) where

import Control.Concurrent.Classy (MonadConc)
import Control.Concurrent.Classy.BoundedChan (BoundedChan, readBoundedChan)
import Polysemy
import Polysemy.Input (Input (..))


runInputBoundedChan
  :: MonadConc m
  => Member (Embed m) r
  => BoundedChan m i
  -> Sem (Input i ': r) a
  -> Sem r a
runInputBoundedChan chan =
  interpret \Input -> embed (readBoundedChan chan)
