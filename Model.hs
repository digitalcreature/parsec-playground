module Model where

type AST = GenAST String

data GenAST a = Nodes [GenAST a] | Node a

instance (Show a) => Show (GenAST a) where
  show (Nodes as) = show as
  show (Node a) = show a
