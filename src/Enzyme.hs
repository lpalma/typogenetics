module Enzyme (act, Enzyme) where

import AminoAcid
import Control.Monad.State
import Strand

type Enzyme = [AminoAcid]

act :: Enzyme -> Strand -> Strand
act _ Empty = Empty
act [] strand = strand
act enzyme strand = fst $ foldl (\(s, p) a -> runState (a s) p) (bind strand) enzyme 
  where bind strand = (strand, 0)
