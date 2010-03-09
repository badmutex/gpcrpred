

import GPCRPred
import GPCRPred.Format
import GPCRPred.Util

import Control.Applicative ((<$>))
import Data.List 
import Text.Printf
import Data.Maybe
import qualified Data.Map as M



tmhmmf = "/home/badi/Research/gpcrs/data/uniprot-organism-aedes.tmhmm"
gpcrhmmf = "/home/badi/Research/gpcrs/data/uniprot-organism-aedes.gpcrhmm"
phobiusf = "/home/badi/Research/gpcrs/data/uniprot-organism-aedes.phobius"



g = doparse gpcrhmmf gpcrhmm
p = doparse phobiusf phobius
t = doparse tmhmmf tmhmm

is = intersection [ gpcrUniprots <$> g
                  , gpcrUniprots <$> p
                  , gpcrUniprots <$> t
                  ]

go = do
  gs <- gpcrUniprotScores fromJust <$> g
  intersection <- is
  let i = filter (\g -> fst g `elem` intersection) gs
      t = printf "|%s|" . confluenceTable . uniprotsScoresColumns M.empty $ i
  writeFile "/tmp/tmp.txt" t