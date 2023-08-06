module Data.Expr (
  Expr (..),
  eval,
) where
import Data.Text (append)

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
