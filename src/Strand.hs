module Strand (Base(..), Strand(..), complement) where

data Base = A | T | G | C
              deriving (Show, Eq)
data Strand = Cons Base Strand
              | S Base Strand
              | Empty
                deriving (Show, Eq)

complement :: Base -> Base
complement A = T
complement T = A
complement G = C
complement C = G
