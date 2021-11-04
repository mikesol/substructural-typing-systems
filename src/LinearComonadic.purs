module LinearComonadic (Timestamp, Stream, Start, start, mkStream, (:<), head, freeze, tail) where

import Prelude

import Data.Tuple (Tuple(..))

data Start

newtype Timestamp (proof :: Type) (v :: Type) = Timestamp v

derive instance functorTimestamp :: Functor (Timestamp proof)
newtype Stream (proof :: Type) (v :: Type) = Stream (Tuple v (Unit -> Stream proof v))

instance functorStream :: Functor (Stream proof) where
  map f (Stream (Tuple a b)) = Stream (Tuple (f a) ((map <<< map) f b))

start :: forall a. a -> Timestamp Start a
start = Timestamp

mkStream
  :: forall proofA a
   . Timestamp proofA a
  -> (forall proofB. Timestamp proofB a -> Stream proofB a)
  -> Stream proofA a
mkStream (Timestamp ts) f = Stream (Tuple ts (\_ -> f (Timestamp ts)))

head :: forall proof a. Stream proof a -> a
head (Stream (Tuple a _)) = a

tail :: forall proof. Stream proof ~> Stream proof
tail (Stream (Tuple _ b)) = b unit

infixr 6 mkStream as :<

freeze :: forall proof. Timestamp proof ~> Stream proof
freeze s = mkStream s freeze
