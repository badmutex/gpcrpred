
module GPCRPred.PredCouple where

import GPCRPred.Types
import GPCRPred.Util

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>))
import Data.Maybe
import Data.Either



data PredCoupleScore = Score { g_q, g_i, g_s, g_12 :: Score } deriving Show


data GProtein = Gq | Gi | Gs | G12 deriving (Eq, Ord, Show)


mkScore xs = 
  let i = map (fromJust . flip lookup xs) [Gq, Gi, Gs, G12]
  in Score { g_q = i !! 0
           , g_i = i !! 1
           , g_s = i !! 2
           , g_12 = i !! 3
           }



begin :: Parser UniprotId
begin = do
  string "Query sequence: "
  letter `manyTill` char '|'
  uid <- alphaNum `manyTill` char '|'
  anyChar `manyTill` newline
  return uid


ignored_midline = do
  string "G-protein coupling specificity - Normalised score"
  newline


nomatch = string "No matches found"

gqP, gsP, giP, g12P :: Parser String
gqP = string "Gq/11"
gsP = string "Gs"
giP = string "Gi/o"
g12P = string "G12/13"


lookupProtein "Gq/11" = Gq
lookupProtein "Gs" = Gs
lookupProtein "Gi/o" = Gi
lookupProtein "G12/13" = G12


ginteraction :: Parser (GProtein, Score)
ginteraction = do
  p <- choice $ map try [gqP, gsP, giP, g12P]
  spaces; char '-'; spaces
  s <- decimal
  return (lookupProtein p, s)


ginteraction_line :: Parser (Maybe (GProtein, Score))
ginteraction_line = (Just <$> ginteraction)
                    <|>
                    (((\_ -> Nothing) <$>) (spaces >> return ""))

gblock :: Parser (Maybe [(GProtein, Score)])
gblock = do
  gs <- catMaybes <$> count 10 (option Nothing ginteraction_line)
  return $ if null gs then Nothing else Just gs


predcouple :: Parser (Result (Maybe PredCoupleScore))
predcouple = do
  uid <- begin
  ignored_midline
  is <- gblock
  return $ if isNothing is
           then Result uid False Nothing
           else Result uid True (Just . mkScore . fromJust $ is)


predcouples :: Parser [Result (Maybe PredCoupleScore)]
predcouples = many (spaces >> predcouple)


t = parse predcouple [] txt
t' = case t of
       Right r -> r
t2 = parse gblock [] "No Matches found"

t3 = do
  s <- readFile "/home/badi/Research/gpcrs/data/uniprot-organism-anopheles.predcouple"
  return $ case (parse predcouples [] s) of
             Left e -> error (show e)
             Right r -> r

txt = "Query sequence: tr|Q7RTK4|Q7RTK4_ANOGA\nG-protein coupling specificity - Normalised score\nGq/11 - 0.98\nGs - 0.80\n\nGi/o - 0.01\nG12/13 - 0.01"