module AffineMonadic (start,extract, bind, discard
 , consume, class GetLexicalLast, allocate, Context, Indexed) where

import Prelude hiding (bind, discard)

import Prim.Row (class Cons)
import Prim.RowList as RL
import Prim.Symbol as Symbol

newtype Indexed (key :: Symbol) (tp :: Type) = Indexed tp
newtype Context (i :: Row Type) (o :: Row Type) (v :: Type) = Context v

start :: Context () () Unit
start = Context unit

extract :: forall a o. Context () o a -> a
extract (Context a) = a

bind :: forall x y z a b. Context x y a -> (a -> Context y z b) -> Context x z b
bind (Context a) f = Context $ let Context b = f a in b

discard :: forall b x y z. Context x y Unit -> (Unit -> Context y z b) -> Context x z b
discard _ f = Context $ let Context b = f unit in b

consume :: forall key a b i o. Cons key Unit o i => (a -> b) -> Indexed key a -> Context i o b
consume f (Indexed a) = Context (f a)

class GetLexicalLast (default :: Symbol) (i :: RL.RowList Type) (s :: Symbol) | default i -> s

instance getLexicalLastNil :: GetLexicalLast sym RL.Nil sym

instance getLexicalLastCons :: GetLexicalLast sym rest o => GetLexicalLast prev (RL.Cons sym val rest) o

allocate
  :: forall sym sym' iRL a i o
   . RL.RowToList i iRL
  => GetLexicalLast "" iRL sym'
  => Symbol.Append sym' "_" sym
  => Cons sym Unit i o
  => a
  -> Context i o (Indexed sym a)
allocate a = Context (Indexed a)
