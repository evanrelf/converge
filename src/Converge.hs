module Converge where

import qualified Converge.Api as Api


main :: IO ()
main = Api.run 7777 "super-secret-code"
