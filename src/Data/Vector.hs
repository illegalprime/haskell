module Data.Vector (
  Vector (..),
  len,
  hd,
  listify,
) where
import GHC.TypeLits (type(+))
import qualified Text.Show

data Vector a (n :: Nat) where
  None :: Vector a 0
  Cons :: a -> Vector a n -> Vector a (1 + n)

instance Show a => Show (Vector a n) where
  show = show . listify

hd :: Vector a (1 + n) -> a
hd (Cons x _) = x

len :: KnownNat n => Vector a n -> Natural
len = natVal

listify :: Vector a n -> [a]
listify None = []
listify (Cons x xs) = x : listify xs
