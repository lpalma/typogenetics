module AminoAcid (del, AminoAcid) where

import Control.Arrow
import Strand

type AminoAcid = (BoundStrand -> BoundStrand)

del :: AminoAcid
del Empty = Empty
del (BoundStrand xs p) = BoundStrand (deleteAt p xs) p


deleteAt :: Int -> [a] -> [a]
deleteAt p = uncurry (++) . second (drop 1) . splitAt p
