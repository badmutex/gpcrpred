
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


nomatch = try (string "No matches found" >> return True) <|> return False

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
  failed <- lookAhead nomatch
  case failed of
    True -> nomatch >> (return $ Result uid False Nothing)
    False -> do
      is <- gblock
      return $ if isNothing is
               then Result uid True Nothing
               else Result uid True (Just . mkScore . fromJust $ is)


predcouples :: Parser [Result (Maybe PredCoupleScore)]
predcouples = manyTill (spaces >> predcouple) eof
