module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import LinearMonadic as LM
import AffineMonadic as AM

main :: Effect Unit
main = do
  log $ LM.extract LM.do
    LM.start
    a <- LM.allocate "a"
    LM.consume (append "hello ") a
  log $ AM.extract AM.do
    AM.start
    a <- AM.allocate "a"
    _ <- AM.allocate "b"
    _ <- AM.allocate "c"
    AM.consume (append "hello ") a
