

import GPCRPred

import Control.Applicative ((<$>))
import Data.List 



tmhmmf = "/home/badi/Research/gpcrs/data/uniprot-organism-anopheles.tmhmm"
gpcrhmmf = "/home/badi/Research/gpcrs/data/uniprot-organism-anopheles.gpcrhmm"
phobiusf = "/home/badi/Research/gpcrs/data/uniprot-organism-anopheles.phobius"



g = doparse gpcrhmmf gpcrhmm
p = doparse phobiusf phobius
t = doparse tmhmmf tmhmm

rs = intersection [ gpcrUniprots <$> g
                  , gpcrUniprots <$> p
                  , gpcrUniprots <$> t
                  ]

