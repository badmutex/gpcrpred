
import Text.ParserCombinators.Parsec
import Statistics.Sample
import Data.Array.Vector



class Result a where
    uniprotId :: a -> String
    seqLength :: a -> Integer
    numPredictedTMH :: a -> Integer


data TMHMMResult = TMHMM {
      tmhmm_uniprotId       :: String
    , tmhmm_seqLength       :: Integer
    , tmhmm_numPredictedTMH :: Integer
    } deriving Show

instance Result TMHMMResult where
    uniprotId = tmhmm_uniprotId
    seqLength = tmhmm_seqLength
    numPredictedTMH = tmhmm_numPredictedTMH



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


tmhmm :: Parser TMHMMResult
tmhmm = do

  u <- lookAhead tmhmm_uniprot_id
  l <- tmhmm_comment tmhmm_length ; newline
  c <- tmhmm_comment tmhmm_num_predicted_tmhs ; newline

  anyChar `manyTill` try (tmhmm_break <|> newline)

  return TMHMM {
               tmhmm_uniprotId = u
             , tmhmm_seqLength = l
             , tmhmm_numPredictedTMH = c
             }


tmhmm_break = spaces >> char '_' `manyTill` newline >> newline
tmhmm_end = do
  string "References"
  anyChar `manyTill` eof

tmhmms :: Parser [TMHMMResult]
tmhmms = do
  string "TMHMM result" >> newline
  spaces >> string "[1]HELP with output formats" >> newline
  tmhmm_break

  many tmhmm







t p s = parse (tmhmm_comment p) [] s

t1 = t tmhmm_length "# tr_Q2HPE8_Q2HPE8_ANOGA Length: 460"
t2 = t tmhmm_num_predicted_tmhs "# tr_Q2HPE8_Q2HPE8_ANOGA Number of predicted TMHs:  7"
t3 = parse tmhmm_uniprot_id [] "# tr_Q2HPE8_Q2HPE8_ANOGA Length: 460"


testf = "/home/badi/Research/gpcrs/tmhmm2/data/test.tmhmm"
testf2 = "/home/badi/Research/gpcrs/tmhmm2/data/test.gpcrhmm"

tmhmmf = "/home/badi/Research/gpcrs/tmhmm2/data/uniprot-organism_anopheles-gpcr.tmhmm"

tmhmm_summarize f = do
  p <- readFile f >>= return . parse tmhmms []
  return $ case p of
             Left e   -> error $ show e
             Right r' -> tmhmm_summary r'


data Summary = Summary { ave, dev :: Double
                       , counts   :: [(Int, Int)]
                       , rest     :: Int
                       , total    :: Int
                       } deriving Show

tmhmm_summary :: [TMHMMResult] -> Summary
tmhmm_summary rs = let vals = toU $ map (fromIntegral . numPredictedTMH) rs
                       counts' = let c = [0..7]
                                 in map (\i -> (fromIntegral i, length $ filter ( (==) i . numPredictedTMH) rs)) c
                   in Summary {
                            ave    = mean vals
                          , dev    = stdDev vals
                          , counts = counts'
                          , rest   = length $ filter ( (> 7) . numPredictedTMH) rs
                          , total  = lengthU vals
                          }
