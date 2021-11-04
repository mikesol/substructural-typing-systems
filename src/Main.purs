module Main where

import Prelude hiding (bind, discard)

import Effect (Effect)
import Effect.Console (log)
import Linear as L

main :: Effect Unit
main = do
  log $ L.extract L.do
    L.start
    a <- L.allocate "a"
    L.consume (append "hello ") a
