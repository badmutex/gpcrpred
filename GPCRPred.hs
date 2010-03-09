
module GPCRPred ( 
                 module GPCRPred.Types
                , G.gpcrhmm
                , P.phobius
                , T.tmhmm
                , parse
                , ParseError
                ) where


import GPCRPred.Types

import qualified GPCRPred.GPCRHMM as G
import qualified GPCRPred.Phobius as P
import qualified GPCRPred.TMHMM as T

import Text.ParserCombinators.Parsec
