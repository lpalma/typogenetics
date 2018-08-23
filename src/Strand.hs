module Strand where

data Base = A | T | G | C
              deriving (Show, Eq)

data Strand = Strand [Base]
              | BoundStrand [Base] Position
              | Empty
                deriving (Show, Eq)

type Position = Int

complement :: Base -> Base
complement A = T
complement T = A
complement G = C
complement C = G
