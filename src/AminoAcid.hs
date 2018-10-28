module AminoAcid ( del
                 , mvr
                 , mvl
                 , AminoAcid) where

import Strand

type AminoAcid = (Strand -> Strand)

del :: AminoAcid
del (BoundStrand xs p) = BoundStrand (deleteAt p xs) p
del Empty = Empty
del (Strand xs) = Strand xs

mvr :: AminoAcid
mvr (BoundStrand xs p) = BoundStrand xs $ p + 1
mvr Empty = Empty
mvr (Strand xs) = Strand xs

mvl :: AminoAcid
mvl (BoundStrand xs p) = BoundStrand xs $ p - 1
mvl Empty = Empty
mvl (Strand xs) = Strand xs

deleteAt :: Int -> [a] -> [a]
deleteAt p xs = yz ++ (drop 1 zs)
                where (yz, zs) = splitAt p xs
