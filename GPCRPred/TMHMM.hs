
module GPCRPred.TMHMM where

import GPCRPred.Types
import GPCRPred.Util

import Text.ParserCombinators.Parsec
import Data.Maybe
import Control.Applicative ((<$>))



comment p = do
  string "# "
  try (string "tr") <|> string "sp"
  char '_'
  p

stringParser :: String -> Parser a -> Parser a
stringParser s p = do
  anyChar `manyTill` try (string s)
  p

seqLength :: Parser Integer
seqLength = stringParser "Length: " (read <$> many digit)

num_predicted_tmhs :: Parser Integer
num_predicted_tmhs = stringParser "Number of predicted TMHs:  " (read <$> many digit)

uniprot_id :: Parser String
uniprot_id = comment $ anyChar `manyTill` char '_'

sectionBreak = spaces >> char '_' `manyTill` newline >> newline

end = do
  string "References"
  anyChar `manyTill` eof


line :: Parser (Result ())
line = do

  u <- lookAhead uniprot_id
  l <- comment seqLength ; newline
  c <- comment num_predicted_tmhs ; newline

  anyChar `manyTill` try (sectionBreak <|> newline)

  return Result {
               uniprotId = u
             , predictedGPCR = c == 7
             , score = ()
             }


tmhmm :: Parser [Result ()]
tmhmm = do
  string "TMHMM result" >> newline
  spaces >> string "[1]HELP with output formats" >> newline
  sectionBreak

  many line