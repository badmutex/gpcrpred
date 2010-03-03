{-# LANGUAGE
  TypeSynonymInstances
  , FlexibleInstances
  #-}

import Text.ParserCombinators.Parsec
import Statistics.Sample
import Data.Array.Vector
import Data.Monoid
import Data.List
import Text.Printf

import Debug.Trace



data Result a = Result {
      uniprotId       :: String
    , predictedGPCR   :: Bool
    , score           :: a
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


tmhmm_break = spaces >> char '_' `manyTill` newline >> newline
tmhmm_end = do
  string "References"
  anyChar `manyTill` eof

tmhmms :: Parser [Result ()]
tmhmms = do
  string "TMHMM result" >> newline
  spaces >> string "[1]HELP with output formats" >> newline
  tmhmm_break

  many tmhmm


data GPCRHMMScore = GPCRHMM { global, local :: Maybe Double } deriving Show


gpcrhmm_head = do
  string "Sequence identifier" ; spaces
  string "global"              ; spaces
  string "local"               ; spaces
  string "pred"                ; spaces



gpcrhmm_line :: Parser (Result GPCRHMMScore)
gpcrhmm_line = do
  letter `manyTill` char '|'
  u <- alphaNum `manyTill` char '|'
  many $ try alphaNum <|> char '_'
  spaces

  scores <- try (string "Too short sequence" >> return Nothing) <|> do
                 global <- decimal ; spaces
                 local  <- try (Just `fmap` decimal) <|> (string "-" >> return Nothing)
                 return $ Just (global, local)

  spaces
  pred <- string "No" <|> string "GPCR"
  spaces

  return Result {
                 uniprotId = u
               , predictedGPCR = pred == "GPCR"
               , score = uncurry GPCRHMM $ case scores of
                                  Nothing -> (Nothing,Nothing)
                                  Just (g, l) -> (Just g, l)
               }

gpcrhmms :: Parser [Result GPCRHMMScore]
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
               , score = ()
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

testf = "/home/badi/Research/gpcrs/data/test.tmhmm"
testf2 = "/home/badi/Research/gpcrs/data/test.gpcrhmm"
testf3 = "/home/badi/Research/gpcrs/data/test.phobius"

tmhmmf = "/home/badi/Research/gpcrs/data/uniprot-organism-anopheles.tmhmm"
gpcrhmmf = "/home/badi/Research/gpcrs/data/uniprot-organism-anopheles.gpcrhmm"
phobiusf = "/home/badi/Research/gpcrs/data/uniprot-organism-anopheles.phobius"

summarize f p = do
  p <- readFile f >>= return . parse p []
  return $ case p of
             Left e   -> error $ show e
             Right r' -> map uniprotId . filter predictedGPCR $ r'



tmhmm_pred = summarize testf2 {- tmhmmf -} tmhmms
gpcrhmm_pred = summarize gpcrhmmf gpcrhmms
phobius_pred = summarize phobiusf phobius

pred_intersect = do
  a <- tmhmm_pred
  b <- gpcrhmm_pred
  c <- phobius_pred
  return $ a `intersect` b `intersect` c

confluenceIntersection :: [String] -> String
confluenceIntersection is = let cs = map toConfluence is
                            in intercalate "\n" cs


uniprotURL :: String -> String
uniprotURL = printf "http://www.uniprot.org/uniprot/%s"

toConfluence :: String -> String
toConfluence s = printf "[%s|%s]" s (uniprotURL s)


-- zipWith' :: (a -> a -> a -> b) -> a -> [a] -> [a] -> [a] -> [b]
-- zipWith' _ _ [] [] [a] = []
-- zipWith' f d [] [] (z:zs) = f d d z : zipWith' f d [] [] zs
-- zipWith' f d (x:xs) []  = f x d : zipWith' f d xs []
-- zipWith' f d (x:xs) (y:ys) = f x y : zipWith' f d xs ys


-- allToConfluence :: [String] -> [String] -> [String] -> String
-- allToConfluence as bs = let con = map toConfluence
--                         in intercalate "\n" $ zipWith' (\a b c -> printf "| %s | %s |" a b) " " (con as) (con bs)

-- go = do
--   g <- tmhmm_pred
--   p <- phobius_pred
--   let c = allToConfluence g p
--   writeFile "/tmp/confluence.txt" c