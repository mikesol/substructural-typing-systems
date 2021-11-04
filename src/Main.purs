module Main where

import Prelude

import AffineMonadic as AM
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinearComonadic (start, head, tail, freeze, (:<))
import LinearMonadic as LM

main :: Effect Unit
main = do
  log $ LM.extract LM.do
    LM.start
    a <- LM.allocate "world"
    LM.consume (append "hello ") a
  log $ AM.extract AM.do
    AM.start
    a <- AM.allocate "all"
    _ <- AM.allocate "b"
    _ <- AM.allocate "c"
    AM.consume (append "goodbye ") a
  let stream = start 1 :< \a -> map ((+) 1) a :< \b -> map ((+) 1) b :< freeze
  let h0 = head stream
  let t0 = tail stream
  let h1 = head t0
  let t1 = tail t0
  let h2 = head t1
  logShow h0
  logShow h1
  logShow h2
