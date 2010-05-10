

import Data.List (intercalate)
import System.FilePath (takeFileName)
import HSH
import Text.Printf
import System.Environment

querycmd :: String -> String -> String -> String
querycmd stderr output fasta =
    printf "curl --stderr %s  -o %s --data-urlencode seq@%s http://athina.biol.uoa.gr/cgi-bin/bioinformatics/PRED-COUPLE2/pc2++.cgi"
                   stderr     output                   fasta

querycmd' = querycmd "file.err" "file.out"

parsecmd :: String -> String -> String
parsecmd parser_py queryfile =
    printf "python %s %s" parser_py queryfile


parsecmd' = parsecmd "parser_xml.py" "file.out"

cleaned = unwords . map (filter (not . (== '-'))) . words

fastaheader fastafile = readFile fastafile >>= return . head . lines

main = do
  [fasta] <- getArgs
  run $ querycmd' fasta :: IO ()
  result  <- fmap (map cleaned . lines) $ run parsecmd' :: IO [String]
  header  <- fastaheader fasta

  mapM_ (flip (printf "!%s!%s!\n") header . intercalate "!" . words) result
