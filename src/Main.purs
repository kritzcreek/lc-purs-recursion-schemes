module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, fold, foldlDefault, foldrDefault)
import Data.Functor.Nu (Nu)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Traversable (class Traversable, sequenceDefault)
import Matryoshka as M

data ExprF a
  = NumLit Int
  | Add a a
  | Div a a

numLit :: Int -> Expr
numLit n = M.embed (NumLit n)

addE :: Expr -> Expr -> Expr
addE l r = M.embed (Add l r)

divE :: Expr -> Expr -> Expr
divE l r = M.embed (Div l r)

derive instance functorExprF :: Functor ExprF

instance foldableExprF :: Foldable ExprF where
  foldl f d = foldlDefault f d
  foldr f d = foldrDefault f d
  foldMap f = case _ of
    NumLit _ -> mempty
    Add l r -> f l <> f r
    Div l r -> f l <> f r

instance traversableExprF :: Traversable ExprF where
  traverse f = case _ of
    NumLit a -> pure (NumLit a)
    Add l r  -> Add <$> f l <*> f r
    Div l r  -> Div <$> f l <*> f r
  sequence = sequenceDefault

type Expr = Nu ExprF

inc :: Expr -> Expr
inc = M.cata case _ of
  NumLit i -> M.embed $ NumLit (i + 1)
  x -> M.embed x

data DivisionByZero = DivisionByZero

instance showDBZ :: Show DivisionByZero where
  show _ = "DivisionByZero"

eval :: Expr -> Either DivisionByZero Int
eval = M.cataM case _ of
  NumLit i -> pure i
  Add l r -> pure (l + r)
  Div l r -> case r of
    0 -> Left DivisionByZero
    _ -> pure (l / r)

myExpr :: Expr
myExpr = divE (addE (numLit 4) (numLit 5)) (numLit 0)

collect :: Expr -> List Int
collect = M.cata case _ of
  NumLit i -> pure i
  x -> fold x

gen :: Int -> Maybe Expr
gen = M.anaM case _ of
  n | n < 0 -> Nothing
  0 -> pure $ NumLit 6
  n -> pure $ Add (n - 1) (n - 1)

showExpr :: Expr -> String
showExpr = M.cata case _ of
  NumLit i -> "(NumLit " <> show i <> ")"
  Add l r -> "(Add " <> l <> " " <> r <> ")"
  Div l r -> "(Div " <> l <> " " <> r <> ")"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (eval myExpr)
  logShow (collect myExpr)
  logShow (map showExpr (gen 2))
