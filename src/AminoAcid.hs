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
rpu strand = withState (rightPurine strand) $ return strand

inc :: AminoAcid
inc Empty = return Empty
inc strand = mapState insertC $ return strand
             where insertC (s, p) = (insertBase p C s, p + 1)

int :: AminoAcid
int Empty = return Empty
int strand = mapState insertT $ return strand
             where insertT (s, p) = (insertBase p T s, p + 1)

cop :: AminoAcid
cop = undefined

swi :: AminoAcid
swi = undefined

lpu :: AminoAcid
lpu Empty = return Empty
lpu strand = withState (leftPurine strand) $ return strand

deleteAt :: Int -> Strand -> Strand
deleteAt _ Empty = Empty
deleteAt 0 (Cons b s) = s
deleteAt p (Cons b s) = Cons b $ deleteAt (p - 1) s

insertBase :: Int -> Base -> Strand -> Strand
insertBase _ _ Empty = Empty
insertBase 0 nb (Cons b s) = Cons b $ Cons nb s
insertBase p nb (Cons b s) = Cons b $ insertBase (p - 1) nb s

rightPurine :: Strand -> Int -> Int
rightPurine Empty p = p
rightPurine strand p = case baseAt strand nextP of
                            Just A -> nextP
                            Just G -> nextP
                            _ -> rightPurine strand (nextP)
                       where nextP = p + 1

leftPurine :: Strand -> Int -> Int
leftPurine Empty p = p
leftPurine strand p = case baseAt strand nextP of
                           Just A -> nextP
                           Just G -> nextP
                           _ -> leftPurine strand nextP
                      where nextP = p - 1

baseAt :: Strand -> Int -> Maybe Base
baseAt Empty _ = Nothing
baseAt (Cons b _) 0 = Just b
baseAt (Cons b s) p = baseAt s (p - 1)
