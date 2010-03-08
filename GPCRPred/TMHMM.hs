
module GPCRPred.TMHMM where

import GPCRPred.Types
import GPCRPred.Util

import Text.ParserCombinators.Parsec
import Data.Maybe
import Control.Applicative ((<$>))



tmhmm_comment p = do
  string "# "
  try (string "tr") <|> string "sp"
  char '_'
  p

tmhmmString :: String -> Parser a -> Parser a
tmhmmString s p = do
  anyChar `manyTill` try (string s)
  p

tmhmm_length :: Parser Integer
tmhmm_length = tmhmmString "Length: " (read <$> many digit)

tmhmm_num_predicted_tmhs :: Parser Integer
tmhmm_num_predicted_tmhs = tmhmmString "Number of predicted TMHs:  " (read <$> many digit)

tmhmm_uniprot_id :: Parser String
tmhmm_uniprot_id = tmhmm_comment $ anyChar `manyTill` char '_'

tmhmm_break = spaces >> char '_' `manyTill` newline >> newline
tmhmm_end = do
  string "References"
  anyChar `manyTill` eof

tmhmms :: Parser [Result ()]
tmhmms = do
  strinnng "TMHMM result" >> newline
  spaces >> string "[1]HELP with output formats" >> newline
  tmhmm_break

  many tmhmm

tmhmm :: Parser (Result ())
tmhmm = do

  u <- lookAhead tmhmm_uniprot_id
  l <- tmhmm_comment tmhmm_length ; newline
  c <- tmhmm_comment tmhmm_num_predicted_tmhs ; newline

  anyChar `manyTill` try (tmhmm_break <|> newline)

  return Result {
               uniprotId = u
             , predictedGPCR = c == 7
             , score = ()
             }
