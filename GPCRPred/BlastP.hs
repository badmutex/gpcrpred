


import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.IntMap (IntMap)
import Data.Char
import Text.Printf

import System.Environment

type BitScore = Double
type Results = IntMap [BitScore]

type SubjIx = Int
type ValIx = Int


addline :: SubjIx -> ValIx -> Text -> Results -> Results
addline subjix valix t = let ws = T.words t
                             subject = ws !! subjix
                             sid = read' . T.filter isDigit
                             read' = read . T.unpack
                         in M.insertWith (++) (sid subject) [read . T.unpack $ ws !! valix]

type Threshold = Double

maxScores :: Threshold -> Results -> [(Int, BitScore)]
maxScores t = filter ((> t) . snd) . M.toList . M.map sum


handleFileData :: SubjIx -> ValIx -> Text -> Results
handleFileData si vi = foldr (addline si vi) M.empty . T.lines

bestResults t = maxScores t


handleFile :: (Results -> [(Int, BitScore)]) -> (Text -> Results) -> FilePath -> IO [(Int, BitScore)]
handleFile getbest texthandler path = T.readFile path >>= return . getbest . texthandler


printBest = uncurry (printf "%d %.2f\n")

main = do
  [subjix, valix, threshold, path] <- getArgs
  handleFile (maxScores (read threshold)) (handleFileData (read subjix) (read valix)) path
  >>= mapM_ printBest



