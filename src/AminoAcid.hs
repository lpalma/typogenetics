module AminoAcid ( del
                 , mvr
                 , mvl
                 , rpu
                 , inc
                 , cop
                 , swi
                 , lpu
                 , int
                 , AminoAcid) where

import Strand
import Control.Monad.State

type AminoAcid = Strand -> State Position Strand
type Position = Int

del :: AminoAcid
del Empty = return Empty
del strand = do position <- get
                newStrand <- return $ deleteAt position strand
                return newStrand

mvr :: AminoAcid
mvr Empty = return Empty
mvr strand = withState (+1) $ return strand

mvl :: AminoAcid
mvl Empty = return Empty
mvl strand = withState toTheLeft $ return strand
             where toTheLeft 0 = 0
                   toTheLeft p = p - 1

rpu :: AminoAcid
rpu = undefined

inc :: AminoAcid
inc = undefined

cop :: AminoAcid
cop = undefined

swi :: AminoAcid
swi = undefined

lpu :: AminoAcid
lpu = undefined

int :: AminoAcid
int = undefined

deleteAt :: Int -> Strand -> Strand
deleteAt _ Empty = Empty
deleteAt 0 (Cons b s) = s
deleteAt p (Cons b s) = Cons b $ deleteAt (p - 1) s
