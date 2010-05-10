{-

This submits a fasta file with ONE sequence in it to predcouple, then prints the results formatted as

!<G protein>!<Specificity>!<fasta header>!

using the '!' as the field separator.

-}

import Data.List (intercalate)
import System.FilePath (takeFileName)
import HSH
import Text.Printf
import System.Environment
import System.Directory

import Data.Default

type Uri = String

data Options = Opts {
      predcouple_url :: Uri
    , curl_err :: FilePath
    , curl_out :: FilePath
    , fasta :: FilePath
    , parser_py :: FilePath
    } deriving (Show)

instance Default Options where
    def = Opts {
            predcouple_url = "http://athina.biol.uoa.gr/cgi-bin/bioinformatics/PRED-COUPLE2/pc2++.cgi"
          , curl_err = "/dev/null"
          , curl_out = "predcouple.out"
          , fasta = "fastafile"
          , parser_py = "parser_xml.py"
          }

validate :: Options -> IO [String]
validate opts = do
  filter (not . null)
  `fmap`
  mapM (check . (\(f, b) -> (f opts, b))) [(curl_err, False), (curl_out, False), (fasta, True), (parser_py, True)]
      where canRead f = getPermissions f >>= return . readable
            canWrite f = getPermissions f >>= return . writable
            check (f, mustexist) = do exists <- doesFileExist f
                                      if mustexist
                                         then do
                                           r <- canRead f
                                           w <- canWrite f
                                           return $ if r && w
                                                      then ""
                                                      else "Cannot read or write " ++ f
                                        else return ""



querycmd :: Options -> String
querycmd opts =
    printf "curl --stderr %s  -o %s --data-urlencode seq@%s http://athina.biol.uoa.gr/cgi-bin/bioinformatics/PRED-COUPLE2/pc2++.cgi"
                (curl_err opts) (curl_out opts)    (fasta opts)

parsecmd :: Options -> String
parsecmd opts =
    printf "python %s %s" (parser_py opts) (curl_out opts)


cleaned = unwords . map (filter (not . (== '-'))) . words

fastaheader fastafile = readFile fastafile >>= return . head . lines

usage = "Usage: <fastafile> <parser.py> [curl outfile]"

getOpts                         :: [String] -> Options
getOpts opts | length opts < 2   = error usage
getOpts [fasta, parser]          = def {fasta = fasta, parser_py = parser}
getOpts [fasta, parser, curlout] = (getOpts [fasta,parser]) {curl_out = curlout}

go :: Options -> IO ()
go opts = do run $ querycmd opts :: IO ()
             result  <- fmap (map cleaned . lines) $ run (parsecmd opts) :: IO [String]
             header  <- fastaheader $ fasta opts

             mapM_ (flip (printf "!%s!%s!\n") header . intercalate "!" . words) result


main = do
  opts   <- return . getOpts =<< getArgs
  errors <- validate opts

  if null errors
    then go opts
    else error $ intercalate "\n" errors

