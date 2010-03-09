
module GPCRPred.Format where

import GPCRPred.Util

uniprotURL :: String -> String
uniprotURL = printf "http://www.uniprot.org/uniprot/%s"

toConfluenceLink :: (String -> String) -> String -> String
toConfluenceLink tolink s = printf "[%s|%s]" s (tolink s)

confluenceTable :: [[String]] -> String
confluenceTable (x:xs) = intercalate "| \n|" $ foldl (zipWith' toConfluence "-") x xs