module ExpressionTree
( ExpressionTree(Op, Value)
, evaluate ) where

data ExpressionTree a = Op (a -> a -> a) (ExpressionTree a) (ExpressionTree a)
              | Value a

evaluate :: ExpressionTree a -> a
evaluate (Value x) = x
evaluate (Op f l r) = f (evaluate l) (evaluate r)