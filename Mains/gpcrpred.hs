

import GPCRPred
import GPCRPred.Format
import GPCRPred.Util
import GPCRPred.PredCouple

import Control.Applicative ((<$>))
import Data.List 
import Text.Printf
import Data.Maybe
import qualified Data.Map as M



tmhmmf   = "/home/badi/Research/gpcrs/data/uniprot-organism-aedes.tmhmm"
gpcrhmmf = "/home/badi/Research/gpcrs/data/uniprot-organism-aedes.gpcrhmm"
phobiusf = "/home/badi/Research/gpcrs/data/uniprot-organism-aedes.phobius"



g = doparse gpcrhmmf gpcrhmm
p = doparse phobiusf phobius
t = doparse tmhmmf tmhmm

joint = unionM joint'

joint' = [ gpcrUniprots <$> g
         , gpcrUniprots <$> p
         , gpcrUniprots <$> t
         ]

u = sequence joint' >>= return . foldl1 union

main = u >>= mapM_ putStrLn

go = do
  gs <- gpcrUniprotScores fromJust <$> g
  combined <- joint
  let i = filter (\g -> fst g `elem` combined) gs
      -- t = printf "|%s|" . confluenceTable . uniprotsScoresColumns M.empty $ i
  -- writeFile "/tmp/tmp.txt" t
  putStrLn (show . length $ map fst i)




predc = do
  s <- readFile "/home/badi/Research/gpcrs/data/uniprot-organism-anopheles.predcouple"
  return $ case (parse predcouples [] s) of
             Left e -> error (show e)
             Right r -> r
