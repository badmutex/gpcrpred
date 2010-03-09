
module GPCRPred.Format where

import GPCRPred.Util

import Text.Printf
import Data.List (intercalate)

uniprotURL :: String -> String
uniprotURL = printf "http://www.uniprot.org/uniprot/%s"

toConfluenceLink :: (String -> String) -> String -> String
toConfluenceLink tolink s = printf "[%s|%s]" s (tolink s)

toConfluence :: String -> String -> String
toConfluence s1 s2 = printf " %s | %s " s1 s2

confluenceTable :: [[String]] -> String
confluenceTable (x:xs) = intercalate "| \n|" $ foldl (zipWith' toConfluence "-") x xs