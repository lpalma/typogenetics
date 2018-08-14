module Base where

data Base = A | T | G | C
            deriving (Show, Eq)

complement :: Base -> Base
complement A = T
complement T = A
complement G = C
complement C = G
