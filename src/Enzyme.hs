module Enzyme (act, Enzyme) where

import AminoAcid
import Strand

type Enzyme = [AminoAcid]

act :: Enzyme -> Strand -> (Strand, Strand)
act = undefined
