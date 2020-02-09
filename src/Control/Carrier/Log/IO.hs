{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Control.Carrier.Log.IO
  ( LogIOC (..)
  , runLogIO
  -- Re-exports
  , module Control.Effect.Log
  )
where


import Control.Algebra ((:+:) (..), alg, handleCoercible)
import Control.Carrier.Lift (Lift, runM, sendM)
import qualified Data.Text.IO as Text

import Control.Effect.Log


runLogIO :: _m a -> IO a
runLogIO = runM . runLogIOC


newtype LogIOC m a = LogIOC { runLogIOC :: m a }
  deriving stock Show
  deriving newtype (Functor, Applicative, Monad)


instance
  ( Algebra sig m
  , Has (Lift IO) sig m
  )
  => Algebra (Log :+: sig) (LogIOC m) where
  alg (R other) = LogIOC (alg (handleCoercible other))
  alg (L (Log severity message k)) = do
    let severityText =
          case severity of
            Info ->
              "[ INFO  ]"
            Warn ->
              "[ WARN  ]"
            Error ->
              "[ ERROR ]"
    sendM @IO (Text.hPutStrLn stderr (severityText <> " " <> message))
    k
