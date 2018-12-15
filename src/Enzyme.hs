module Enzyme (act, Enzyme) where

import AminoAcid
import Strand

type Enzyme = [AminoAcid]

act :: Enzyme -> Strand -> Strand
act _ Empty = Empty
act _ _ = undefined
