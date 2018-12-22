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
mvl strand = withState moveLeft $ return strand
             where moveLeft 0 = 0
                   moveLeft p = p - 1

rpu :: AminoAcid
rpu Empty = return Empty
rpu strand = withState (nextPurine strand) $ return strand

inc :: AminoAcid
inc Empty = return Empty
inc strand = mapState insertC $ return strand
      where insertC (s, p) = (insertBase p C s, p + 1)

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

insertBase :: Int -> Base -> Strand -> Strand
insertBase _ _ Empty = Empty
insertBase 0 nb (Cons b s) = Cons b $ Cons nb s
insertBase p nb (Cons b s) = Cons b $ insertBase (p - 1) nb s

nextPurine :: Strand -> Int -> Int
nextPurine Empty p = p
nextPurine (Cons b s) p = case base s of
                               Just A -> nextP
                               Just G -> nextP
                               _ -> nextPurine s (nextP)
                          where nextP = p + 1

base :: Strand -> Maybe Base
base Empty = Nothing
base (Cons b _) = Just b
