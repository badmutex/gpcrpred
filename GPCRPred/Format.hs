{-# LANGUAGE NoMonomorphismRestriction #-}
module GPCRPred.Format where

import GPCRPred.Util

import Text.Printf
import Data.List (intercalate, foldl', foldl1', sortBy)
import qualified Data.Map as M

uniprotURL :: String -> String
uniprotURL = printf "http://www.uniprot.org/uniprot/%s"

toConfluenceLink :: (String -> String) -> String -> String
toConfluenceLink tolink s = printf "[%s|%s]" s (tolink s)

toConfluence :: String -> String -> String
toConfluence s1 s2 = printf " %s | %s " s1 s2

confluenceTable :: [[String]] -> String
confluenceTable xs = intercalate "| \n|" $ foldl1' (zipWith' toConfluence "-") xs

uniprotsScoresColumns :: M.Map String String -> [(String, Double)] -> [[String]]
uniprotsScoresColumns tab = foldl' (\[ids,ss,tasser] (uid, s) -> [ toConfluenceLink uniprotURL uid : ids
                                                                 , printf "%.2f" s : ss
                                                                 , maybe (printf "[%s]" uid) id (M.lookup uid tab) : tasser
                                                                 ]) [[],[],[]]
                        . sortBy (\(_,l) (_,r)-> compare l r)

