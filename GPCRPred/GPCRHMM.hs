
module GPCRPred.GPCRHMM where

import GPCRPred.Types
import GPCRPred.Util

import Text.ParserCombinators.Parsec
import Data.Maybe
import Control.Applicative ((<$>))

data GPCRHMMScore = Score { global, local :: Maybe Double } deriving Show

instance CompositScore GPCRHMMScore where
    compositScore s = case (global s, local s) of
                        (Just g, Just l) -> Just $ g + l
                        _ -> Nothing


header = do
  string "Sequence identifier" ; spaces
  string "global"              ; spaces
  string "local"               ; spaces
  string "pred"                ; spaces

line = do
  letter `manyTill` char '|'
  u <- alphaNum `manyTill` char '|'
  many $ try alphaNum <|> char '_'
  spaces

  scores <- try (string "Too short sequence" >> return Nothing) <|> do
                 global <- decimal ; spaces
                 local  <- try (Just <$> decimal) <|> (string "-" >> return Nothing)
                 return $ Just (global, local)

  spaces
  pred <- string "No" <|> string "GPCR"
  spaces

  return Result {
                 uniprotId = u
               , predictedGPCR = pred == "GPCR"
               , score = uncurry Score $ case scores of
                                  Nothing -> (Nothing,Nothing)
                                  Just (g, l) -> (Just g, l)
               }


gpcrhmm :: Parser [Result GPCRHMMScore]
gpcrhmm = do
  header
  many line


testf = "/home/badi/Research/gpcrs/data/test.gpcrhmm"
t = readFile testf >>= return . parse gpcrhmm [] 