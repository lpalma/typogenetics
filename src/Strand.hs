module Strand (Base(..)
              , Strand(..)
              , complement
              , fromList
              , fromLists) where

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

fromList :: [Base] -> Strand
fromList = foldr Cons Empty

fromLists :: [[Base]] -> Strand
fromLists (x:[]) = S (head x) $ fromList (tail x)
fromLists (x:xs) = foldr Cons (fromLists xs) x
