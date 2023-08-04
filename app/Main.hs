module Main where
import GHC.TypeLits (type(+))
import Data.Text (append)

-- Messing w/ GADTs
type Expr :: Type -> Type
data Expr a where
  I :: Integer -> Expr Integer
  S :: Text -> Expr Text
  Add :: Expr Integer -> Expr Integer -> Expr Integer
  Cat :: Expr Text -> Expr Text -> Expr Text
  ToString :: Show a => Expr a -> Expr Text

eval :: Expr a -> a
eval (I i) = i
eval (S s) = s
eval (Add a b) = eval a + eval b
eval (Cat a b) = eval a `append` eval b
eval (ToString o) = show . eval $ o

-- Messing w/ Kinds
data Vector a (n :: Nat) where
  None :: Vector a 0
  Cons :: a -> Vector a n -> Vector a (1 + n)

hd :: Vector a (1 + n) -> a
hd (Cons x _) = x

len :: KnownNat n => Vector a n -> Natural
len = natVal

main :: IO ()
main = putStrLn "Hello, Haskell!"
