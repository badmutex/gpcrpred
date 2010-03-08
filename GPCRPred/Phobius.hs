
module GPCRPred.Phobius where

import GPCRPred.Types
import GPCRPred.Util

import Text.ParserCombinators.Parsec
import Data.Maybe
import Control.Applicative ((<$>))




header = do
  string "SEQENCE ID" ; spaces
  string "TM"          ; spaces
  string "SP"          ; spaces
  string "PREDICTION"  ; spaces

line = do
  letter `manyTill` char '|'
  u <- alphaNum `manyTill` char '|'
  many $ try alphaNum <|> char '_'
  spaces
  p <- read <$> positiveInt
  anyChar `manyTill` newline
  return Result {
                 uniprotId = u
               , predictedGPCR = p == 7
               , score = ()
               }


phobius :: Parser [Result ()]
phobius = do
  header
  many line