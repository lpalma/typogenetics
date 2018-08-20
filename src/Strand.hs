module Strand where

data Base = A | T | G | C
              deriving (Show, Eq)

data BoundStrand = BoundStrand Strand Position
                 | Empty
                     deriving (Show, Eq)

type Strand = [Base]
type Position = Int

complement :: Base -> Base
complement A = T
complement T = A
complement G = C
complement C = G
