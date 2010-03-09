
module GPCRPred.Types where


data Result a = Result {
      uniprotId       :: String
    , predictedGPCR   :: Bool
    , score           :: a
    } deriving Show


class CompositScore a where
    compositScore :: a -> Maybe Double


type UniprotId = String
type Score = Double