{-# LANGUAGE
  TypeSynonymInstances
  , FlexibleInstances
  #-}


module GPCRPred.Util where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>))


positiveInt = many digit
negativeInt = do l <- char '-'
                 r <- positiveInt
                 return $ l : r
 
-- | Parse (-/+) integers
integral :: (Integral i, Read i) => Parser i
integral = (negativeInt <|> positiveInt) >>= return . read 
 
 
decimal :: (Read f, Fractional f) => Parser f
decimal = do
  l <- show <$> integral
  char '.'
  r <- positiveInt
  return . read $ l ++ "." ++ r
  <?> "a fractional number"

