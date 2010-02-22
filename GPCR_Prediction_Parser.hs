{-# LANGUAGE
  TypeSynonymInstances
  , FlexibleInstances
  #-}

import Text.ParserCombinators.Parsec
import Statistics.Sample
import Data.Array.Vector
import Data.Monoid
import Debug.Trace



data Result = Result {
      uniprotId       :: String
    -- , seqLength       :: Integer
    , predictedGPCR   :: Bool
    } deriving Show


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
tmhmm_length = tmhmmString "Length: " (read `fmap` many digit)

tmhmm_num_predicted_tmhs :: Parser Integer
tmhmm_num_predicted_tmhs = tmhmmString "Number of predicted TMHs:  " (read `fmap` many digit)

tmhmm_uniprot_id :: Parser String
tmhmm_uniprot_id = tmhmm_comment $ anyChar `manyTill` char '_'


tmhmm :: Parser Result
tmhmm = do

  u <- lookAhead tmhmm_uniprot_id
  l <- tmhmm_comment tmhmm_length ; newline
  c <- tmhmm_comment tmhmm_num_predicted_tmhs ; newline

  anyChar `manyTill` try (tmhmm_break <|> newline)

  return Result {
               uniprotId = u
             , predictedGPCR = c == 7
             }


tmhmm_break = spaces >> char '_' `manyTill` newline >> newline
tmhmm_end = do
  string "References"
  anyChar `manyTill` eof

tmhmms :: Parser [Result]
tmhmms = do
  string "TMHMM result" >> newline
  spaces >> string "[1]HELP with output formats" >> newline
  tmhmm_break

  many tmhmm


gpcrhmm_head = do
  string "Sequence identifier"
  spaces
  string "global"
  spaces
  string "local"
  spaces
  string "pred"
  spaces

gpcrhmm_line :: Parser Result
gpcrhmm_line = do
  letter `manyTill` char '|'
  u <- alphaNum `manyTill` char '|'
  many $ try alphaNum <|> char '_'
  spaces
  try (show `fmap` decimal) <|> string "Too short"
  spaces
  s <- string "sequence" <|> string "-" <|> (show `fmap` decimal)
  spaces
  pred <- string "No" <|> string "GPCR"
  spaces
  return Result {
                 uniprotId = u
               , predictedGPCR = pred == "GPCR"
               }

gpcrhmms :: Parser [Result]
gpcrhmms = do
  gpcrhmm_head
  many gpcrhmm_line


phobius_header = do
  string "SEQENCE ID" ; spaces
  string "TM"          ; spaces
  string "SP"          ; spaces
  string "PREDICTION"  ; spaces

phobius_line = do
  letter `manyTill` char '|'
  u <- alphaNum `manyTill` char '|'
  many $ try alphaNum <|> char '_'
  spaces
  p <- read `fmap` positiveInt
  anyChar `manyTill` newline
  return Result {
                 uniprotId = u
               , predictedGPCR = p == 7
               }


phobius = do
  phobius_header
  many phobius_line


t p s = parse (tmhmm_comment p) [] s

t1 = t tmhmm_length "# tr_Q2HPE8_Q2HPE8_ANOGA Length: 460"
t2 = t tmhmm_num_predicted_tmhs "# tr_Q2HPE8_Q2HPE8_ANOGA Number of predicted TMHs:  7"
t3 = parse tmhmm_uniprot_id [] "# tr_Q2HPE8_Q2HPE8_ANOGA Length: 460"
t4 = parse gpcrhmm_head [] "Sequence identifier              global     local      pred "
t5 = parse gpcrhmm_line [] "tr|A0NC14|A0NC14_ANOGA           79.31      63.44      GPCR "
t6 = parse phobius_header [] "SEQENCE ID                     TM SP PREDICTION"
t7 = parse phobius_line [] "tr|A0NAM5|A0NAM5_ANOGA          1  0 o26-50i\n"

testf = "/home/badi/Research/gpcrs/tmhmm2/data/test.tmhmm"
testf2 = "/home/badi/Research/gpcrs/tmhmm2/data/test.gpcrhmm"
testf3 = "/home/badi/Research/gpcrs/tmhmm2/data/test.phobius"

tmhmmf = "/home/badi/Research/gpcrs/tmhmm2/data/uniprot-organism_anopheles-gpcr.tmhmm"
gpcrhmmf = "/home/badi/Research/gpcrs/tmhmm2/data/uniprot-organism_anopheles-gpcr.gpcrhmm"
phobiusf = "/home/badi/Research/gpcrs/tmhmm2/data/uniprot-organism_anopheles-gpcr.phobius"

summarize f p = do
  p <- readFile f >>= return . parse p []
  return $ case p of
             Left e   -> error $ show e
             Right r' -> length . filter predictedGPCR $ r'



tmhmm_pred = summarize tmhmmf tmhmms
gpcrhmm_pred = summarize gpcrhmmf gpcrhmms
phobius_pred = summarize phobiusf phobius