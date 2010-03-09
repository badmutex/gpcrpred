{-# LANGUAGE
  TypeSynonymInstances
  , NoMonomorphismRestriction
  , FlexibleInstances
  #-}


module GPCRPred.Util where

import GPCRPred.Types

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>))
import Data.Monoid
import Data.List (foldl1', intersect)


-- positiveInt = many digit
-- negativeInt = do l <- char '-'
--                  r <- positiveInt
--                  return $ l : r

-- -- | Parse (-/+) integers
-- integral :: (Integral i, Read i) => Parser i
-- integral = (negativeInt <|> positiveInt) >>= return . read


-- decimal :: (Read f, Fractional f) => Parser f
-- decimal = do
--   l <- show <$> integral
--   char '.'
--   r <- positiveInt
--   return . read $ l ++ "." ++ r
--   <?> "a fractional number"


instance Monoid (Parser String) where
    mempty = option "" (many anyChar)
    mappend p1 p2 = do
      p1' <- p1
      p2' <- p2
      return $ p1' ++ p2'

(<++>) :: (Monoid a) => a -> a -> a
l <++> r = mconcat [l,r]


positiveInt = many digit
negativeInt = option "" (string "-") <++> positiveInt

-- | Parse (-/+) integers
integral :: (Integral i, Read i) => Parser i
integral = (negativeInt <|> positiveInt) >>= return . read 


decimal :: (Read f, Fractional f) => Parser f
decimal = do
  pre <- option "" (string "-")
  ds <- many digit
  char '.'
  ds' <- many digit
  return . read $ pre ++ ds ++ "." ++ ds'
  <?> "a fractional number"


zipWith' :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
zipWith' _ _ [] [] = []
zipWith' f d [] (x:xs) = f d x : zipWith' f d [] xs
zipWith' f d (x:xs) [] = f x d : zipWith' f d xs []
zipWith' f d (x:xs) (y:ys) = f x y : zipWith' f d xs ys


intersection :: (Eq a, Functor f, Monad f) => [f [a]] -> f [a]
intersection = fmap (foldl1' intersect) . sequence

gpcrUniprots :: [Result a] -> [String]
gpcrUniprots = map uniprotId . filter predictedGPCR
