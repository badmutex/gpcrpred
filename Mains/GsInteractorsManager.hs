#!/usr/bin/env runhaskell

{-# LANGUAGE
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  NoMonomorphismRestriction,
  PackageImports,
  TemplateHaskell,
  TypeOperators
  #-}


import Prelude hiding ((.), id, mod)

import "mtl" Control.Monad.Reader (ReaderT, MonadReader, MonadIO, runReaderT, ask, liftIO)
import "mtl" Control.Monad.Trans
import Control.Applicative ((<$>))
import Control.Category
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad --  (when, liftM)
import Control.Parallel.Strategies
import Data.Char (isSpace)
import Data.Default
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Record.Label
import Debug.Trace (trace)
import GHC.Conc (numCapabilities)
import HSH hiding (space)
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO
import Text.HTML.TagSoup (Tag, parseTags, maybeTagText)
import Text.Parsec.ByteString.Lazy
import Text.ParserCombinators.Parsec hiding (Parser, label)
import Text.Printf (printf)
import Text.StringLike
import qualified Control.Monad.Parallel as PM
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO.Unsafe
import qualified Data.Map as M
import Data.Map (Map)
import System.Console.GetOpt

{-# NOINLINE loggerchan #-}
loggerchan :: Chan (Maybe (Handle, String))
loggerchan = unsafePerformIO $ newChan


dopar = True
pMapM = if dopar then cpMapM numCapabilities else mapM

chunk :: (Integral i) => i -> [a] -> [[a]]
chunk n xs = chunk' i xs
      where
        chunk' _ [] = []
        chunk' n xs = a : chunk' n b where (a,b) = splitAt n xs
        i = ceiling (fromIntegral (length xs) / fromIntegral n)

cpMapM :: (Integral i, Functor f, PM.MonadParallel f) => i -> (a -> f b) -> [a] -> f [b]
cpMapM size f = fmap concat . PM.mapM (mapM f) . chunk size


loggerLoop :: Chan (Maybe (Handle, String)) -> IO ()
loggerLoop c = readChan c >>= \r -> case r of
                                      Nothing -> return ()
                                      Just (h, msg) -> hPutStr h msg >> loggerLoop c

-- | Top level function.
withLogger chan m = (liftIO $ forkIO $ loggerLoop chan)
                    >> m >>
                    (liftIO $ writeChan chan Nothing)




class Show a => Tabular a where
    table :: a -> String


class Truth a where
    truth :: a -> Bool

instance Truth [a] where
    truth [] = False
    truth _  = True

instance Truth (Maybe a) where
    truth Nothing = False
    truth _       = True

if' p a b = if truth p then a else b



type URL       = String
type XML       = String
type UniprotID = String
type Protein   = String
type Verbosity = Bool

data Params = Params {
      _predcouple_training_file :: FilePath
    , _gprotein                 :: Protein
    , _browser                  :: FilePath
    , _modbase_search           :: URL
    , _modbase_retrieve         :: URL
    , _modbase_model_root       :: FilePath
    , _force                    :: Bool
    , _limit_model_pdbs         :: Int
    , _limit_models             :: Int
    , _tmScoreParams            :: TMScoreParams
    , _loggerChannel            :: Chan (Maybe (Handle, String))
    , _verbose                  :: Verbosity
    }




data TMScoreParams = TMScore {
      _tmScoreBin           :: FilePath
    , _tmScoreDzero         :: Maybe Int
    , _tmScoreRasmol        :: Bool
    , _tmScoreOutputDir     :: FilePath
    , _tmScoreOutputSummary :: FilePath
    } deriving Show


data TMScoreResult = TMScoreResult {
                       _tmscoreModel  :: String
                     , _tmscoreNative :: String
                     , _tmscore       :: Double
                     } deriving (Eq, Show)

-- | TMScore is not associative, so store the results for both directions
data TMScorePair = Replicates { tmscoreReplicas :: [TMScoreResult] } deriving (Eq, Show)


$(mkLabels [''Params, ''TMScoreParams, ''TMScoreResult])

predcouple_training_file :: Params :-> FilePath
gprotein                 :: Params :-> Protein
browser                  :: Params :-> FilePath
modbase_search           :: Params :-> URL
modbase_retrieve         :: Params :-> URL
modbase_model_root       :: Params :-> FilePath
force                    :: Params :-> Bool
limit_model_pdbs         :: Params :-> Int
limit_models             :: Params :-> Int
tmScoreParams            :: Params :-> TMScoreParams
loggerChannel            :: Params :-> Chan (Maybe (Handle, String))
verbose                  :: Params :-> Bool

tmScoreBin               :: TMScoreParams :-> FilePath
tmScoreDzero             :: TMScoreParams :-> Maybe Int
tmScoreRasmol            :: TMScoreParams :-> Bool
tmScoreOutputDir         :: TMScoreParams :-> FilePath
tmScoreOutputSummary     :: TMScoreParams :-> FilePath

tmscore                  :: TMScoreResult :-> Double
tmscoreModel             :: TMScoreResult :-> String
tmscoreNative            :: TMScoreResult :-> String 





writeLog :: Handle -> String -> String -> Execute ()
writeLog h fmt msg = 
    let msg' = printf "[%s] %s" fmt msg
    in do chan <- get loggerChannel <$> ask
          liftIO $ writeChan chan (Just (h, msg'))







instance Default TMScoreParams where
    def = set tmScoreBin       "/home/badi/Research/gpcrs/gs-gpcrs/TMscore"                                     $
          set tmScoreDzero     Nothing                                                                          $
          set tmScoreRasmol    True                                                                             $
          (\params -> set tmScoreOutputSummary (flip (</>) "summary.txt" (get tmScoreOutputDir params)) params) $
          set tmScoreOutputDir "/home/badi/Research/gpcrs/gs-gpcrs/tmscore-output"                              $
          TMScore undefined undefined undefined undefined undefined


instance Default TMScoreResult where
    def = set tmscore       0      $
          set tmscoreModel  mempty $
          set tmscoreNative mempty $
          TMScoreResult undefined undefined undefined

instance Ord TMScoreResult where
    compare r1 r2 = compare x y where [x,y] = map (get tmscore) [r1,r2]


instance Tabular TMScoreResult where
    table r = printf "%s\t%s\t%f\n" m n s
        where m = get tmscoreModel r
              n = get tmscoreNative r
              s = get tmscore r

instance Tabular [TMScoreResult] where
    table = concatMap table

instance Ord TMScorePair where
    compare (Replicates r1) (Replicates r2) = compare r1 r2

instance Tabular TMScorePair where
    table (Replicates xs) = reps
        where reps = concatMap table xs

instance Tabular [TMScorePair] where
    table = concatMap table

instance Tabular [[TMScorePair]] where
    table = concatMap table



instance Default Params where
    def = Params {
            _predcouple_training_file = "/home/badi/Research/gpcrs/gs-gpcrs/predcouple-training.txt"
          , _gprotein                 = "gs"
          , _browser                  = "chromium"
          , _modbase_search           = "http://salilab.org/modbase-cgi/model_search.cgi?searchkw=name&kword="
          , _modbase_retrieve         = "http://salilab.org/modbase/retrieve/modbase/?databaseID="
          , _modbase_model_root       = "/home/badi/Research/gpcrs/gs-gpcrs/modbase-models"
          , _force                    = False
          , _limit_model_pdbs         = -1
          , _limit_models             = -1
          , _tmScoreParams            = def
          , _loggerChannel            = loggerchan
          , _verbose                  = False
          }

with = def

tmScoreResult :: BS.ByteString -> Maybe TMScoreResult
tmScoreResult bs = case parse tmScoreParser [] bs of
                     Left _ -> Nothing
                     Right r -> Just r

tmScoreParser_TMScore :: Parser String
tmScoreParser_TMScore = do
  string "TM-score"
  space `manyTill` char '='
  space `manyTill` string "0."
  n <- many digit
  return $ "0." ++ n

tmScoreParser = do
  let getter = choice [tmScoreParser_TMScore, anyChar `manyTill` newline >> getter]
  score <- read <$> getter

  return $ set tmscore score def


runTMScore :: FilePath -> FilePath -> Execute (Maybe TMScorePair)
runTMScore pdb1 pdb2 = do
  [binary, dir] <- mapM (\f -> get (f . tmScoreParams) <$> ask) [ tmScoreBin, tmScoreOutputDir]
  dorasmol      <- get (tmScoreRasmol . tmScoreParams) <$> ask

  liftIO $ createDirectoryIfMissing True dir
  let base       = dropExtension . takeFileName
      cmd f1 f2  = printf "%s %s %s" binary f1 f2 :: String
      cmd1       = cmd pdb1 pdb2
      cmd2       = cmd pdb2 pdb1

      mk (cmd, model, native) = liftIO $ do
                                  let setter r   = set tmscoreModel (takeFileName model) $
                                                   set tmscoreNative (takeFileName native) $
                                                   r
                                      rasmolname = printf "%s-%s.rasmol" (base model) (base native)
                                      cmd'       = if dorasmol
                                                   then printf "%s -o %s" cmd (dir</> rasmolname)
                                                   else cmd

                                  r <- tmScoreResult <$> run cmd' :: IO (Maybe TMScoreResult)
                                  return $ case r of
                                             Just r    -> Just $ setter r
                                             otherwise -> Nothing

  r <- pMapM mk [(cmd1, pdb1, pdb2), (cmd2, pdb2, pdb1)]
  return $ if null r then Nothing else Just . Replicates $ catMaybes r





modelTMScores :: UniprotID -> Execute [TMScorePair]
modelTMScores idno = do
  root   <- get modbase_model_root <$> ask
  models <- liftIO . glob $ printf "%s/%s.*.pdb" root idno

  writeLog stdout "modelTMScores" $ printf "Running TMScore on %s\n" idno

  r <- filter (not . null . tmscoreReplicas) . catMaybes <$>
                     pMapM (uncurry runTMScore) [ (m1,m2) | m1 <- models, m2 <- models, m1 /= m2 ]
  return r


orderedModel f f' uid = do scores <- modelTMScores uid
                           return $ if' scores
                                        (Just . f' . tmscoreReplicas . f $ scores)
                                        Nothing
                           -- return . f' . tmscoreReplicas . f $ scores

worstModel :: UniprotID -> Execute (Maybe TMScoreResult)
worstModel = orderedModel minimum minimum

bestModel :: UniprotID -> Execute (Maybe TMScoreResult)
bestModel = orderedModel maximum maximum

allTMScores :: Execute [[TMScorePair]]
allTMScores = pMapM modelTMScores =<< predcouple_uniprot_ids


orderedModels f = predcouple_uniprot_ids >>= fmap catMaybes . pMapM f >>= copyChosenModels


copyChosenModels ms = do
  pdbdir      <- get modbase_model_root <$> ask
  resultsdir  <- get (tmScoreOutputDir . tmScoreParams) <$> ask
  summaryfile <- get (tmScoreOutputSummary . tmScoreParams) <$> ask
  let names         = map (uncurry get) [ (fn, r) | fn <- [tmscoreModel] , r  <- ms ]
      absolute root = map ((</>) root) names
      sources       = absolute pdbdir
      targets       = absolute resultsdir
      pairs         = zip sources targets

  liftIO $ do
    createDirectoryIfMissing True resultsdir
    mapM_ (uncurry copyFile) pairs
    appendFile summaryfile $ table ms

  return ms


allWorstModels :: Execute [TMScoreResult]
allWorstModels = orderedModels worstModel

allBestModels :: Execute [TMScoreResult]
allBestModels = orderedModels bestModel


test = do
  let f1 = "/home/badi/Research/gpcrs/gs-gpcrs/modebase-models/Q61616.0b114a7c51da720de7927953c5d55036.pdb"
      f2 = "/home/badi/Research/gpcrs/gs-gpcrs/modebase-models/Q61616.af661614b963b06d6a2a6d10b8169ed7.pdb"
      s  = BS.pack "TM-score    = 0.7592  (d0= 6.49, TM10= 0.7409)"
  -- in parse tmScoreParser_TMScore  [] s
  -- exec' $ runTMScore f1 f2
  withLogger loggerchan $
              exec' $ bestModel "Q64326" -- P30083"





data ModbaseModel = Model { model_id :: BS.ByteString
                          , model_pdb :: BS.ByteString
                          } deriving Show

-- instance NFData ModbaseModel

instance Default ModbaseModel where
    def = Model { model_id = BS.empty, model_pdb = BS.empty }




newtype Execute a = Exec {
      unExec :: ReaderT Params IO a
    } deriving (Functor, Monad, MonadReader Params, MonadIO)



instance PM.MonadParallel Execute where
    bindM2 f ma mb = do p  <- ask
                        vb <- liftIO    $ newEmptyMVar
                        liftIO . forkIO $ (exec p mb >>= putMVar vb)
                        a  <- liftIO    $ exec p ma                     
                        b  <- liftIO    $ takeMVar vb
                        f a b


instance RunResult (Execute ()) where
    run cmd = liftIO $ run cmd



exec :: Params -> Execute a -> IO a
exec params = flip runReaderT params . unExec

exec' = exec def



predcouple_uniprot_ids :: Execute [UniprotID]
predcouple_uniprot_ids = do
  f <- get predcouple_training_file <$> ask
  p <- get gprotein                 <$> ask
  let get           =  foldr uniprot []
      uniprot l ids = let ws = words l
                          id = head ws
                          g  = last ws
                      in if g == p then id : ids else ids

  liftIO $ get . lines <$> readFile f


wrapWith :: Monad m => m a -> (a -> m b) -> m a
wrapWith m f = m >>= f >> m



create_url :: URL -> UniprotID -> URL
create_url base = printf "%s%s" base

create_urls :: URL -> [UniprotID] -> [URL]
create_urls base ids = map (create_url base) ids `using` parListChunk numCapabilities rdeepseq

ids_urls :: (Params -> URL) -> Execute [(UniprotID, URL)]
ids_urls accessor = do
  ids  <- predcouple_uniprot_ids
  base <- accessor <$> ask
  return $ map (\id -> (id, create_url base id)) ids 



search_modbase :: Execute ()
search_modbase = do
  urls <- map snd <$> ids_urls (get modbase_search)
  exe  <- get browser <$> ask

  let browse url = let cmd = printf "%s '%s'" exe url :: String
                   in run cmd :: IO ()
  liftIO $ pMapM browse urls >> return ()


whenNotExists checker m = do
  exists <- liftIO $ checker
  if exists
    then return ()
    else m

whenFileNotExists f m = whenNotExists (doesFileExist      f) m
whenDirNotExists  f m = whenNotExists (doesDirectoryExist f) m


retrieveURL :: Verbosity -> URL -> FilePath -> IO FilePath
retrieveURL verbose url file = let cmd = printf "wget -O '%s' '%s'" file url :: String
                                   cmd' = if verbose then cmd else cmd ++ " >/dev/null 2>&1"
                               in do
                                 createDirectoryIfMissing True (dirname file)
                                 run cmd' :: IO ()
                                 return file
  


modbase_retrieve_model :: (UniprotID, URL) -> Execute FilePath
modbase_retrieve_model (idstr,url) = do
  verbose <- get verbose <$> ask
  file  <- (flip (<.>) "xml" . flip (</>) idstr) . get modbase_model_root <$> ask
  force <- get force                                                      <$> ask

  let logger = flip writeLog "modbase_retrieve_model"
  
  liftIO $ createDirectoryIfMissing True (dropFileName file)
  when verbose $ logger stdout $ printf "Created %s\n" (dropFileName file)
  whenFileNotExists file $ do
    when verbose $ logger stdout $ printf "Acquiring %s\n" url
    liftIO $ retrieveURL verbose url file >> return ()
  when verbose $ logger stderr $ printf "Got %s\n" url
  return file

take' :: Int -> [a] -> [a]
take' n = if n < 0 then id else take n

modbase_retrieve_models :: Execute [FilePath]
modbase_retrieve_models = do
  nmodels <- get limit_models     <$> ask
  ids_urls (get modbase_retrieve) >>= pMapM modbase_retrieve_model . take' nmodels


extract_tags :: FilePath -> IO [Tag BS.ByteString]
extract_tags = fmap parseTags . BS.readFile 

trim      :: BS.ByteString -> BS.ByteString
trim      = f . f
   where f = BS.reverse . BS.dropWhile isSpace

-- instance NFData BS.ByteString

extract_text :: FilePath -> IO [BS.ByteString]
extract_text = fmap (filter (not . BS.null) . mapper trim . catMaybes . map maybeTagText) . extract_tags
    where mapper = if dopar then parMap rwhnf else map


modbase_models :: [BS.ByteString] -> [ModbaseModel]
modbase_models = foldl mk [def]
    where
      mk (m:ms) str = let bs = str
                      in case (BS.null $ model_id m, BS.null $ model_pdb m) of
                           (True, True)   -> m { model_id    = bs } : ms
                           (False, True)  -> m { model_pdb   = bs } : ms
                           (False, False) -> with { model_id = bs } : m : ms
                           otherwise      -> error $
                                             printf "Cannot make ModbaseModel from string %s with current model %s"
                                                   (show str)
                                                   (show m)


write_modbase_pdbs :: FilePath -> Execute [FilePath]
write_modbase_pdbs xmlfile = do
  n     <- get limit_model_pdbs <$> ask
  force <- get force            <$> ask
  -- log   <- getLogger "search_modbase"

  liftIO $ par . modbase_models <$> extract_text xmlfile >>= fmap catMaybes . pMapM (writepdb log force) . take' n
    where
      par = if dopar then withStrategy (parListChunk numCapabilities rwhnf) else id
      writepdb log force model = let pdb = replaceExtension xmlfile (printf "%s.pdb" (BS.unpack $ model_id model))
                                 in do path <- newIORef Nothing
                                       when (not . BS.null $ model_pdb model) $ do
                                         BS.writeFile pdb (model_pdb model)
                                         -- log $ printf "Wrote %s\n" pdb
                                         writeIORef path (Just pdb)
                                       return =<< readIORef path

get_and_write_modbase_pdbs :: Execute [FilePath]
get_and_write_modbase_pdbs = do
    force <- get force      <$> ask
    modbase_retrieve_models >>= liftM concat . pMapM write_modbase_pdbs



type PDBID = String

experimentalPDBs :: Map UniprotID PDBID
experimentalPDBs = M.fromList [ ("P29274","3EML") -- Y A2A adenosine receptor bound to antagonist
                              , ("P07550","2R4R") -- Y B2 Adrenergic receptor
                              -- , ("P34998","3EHU") -- N Extracellular domain
                              -- , ("Q60748","1U34") -- N Extracellular domain
                              -- , ("P23945","1XWD") -- N Extracellular domain
                              -- , ("P41586","2JOD") -- N extracellular fragment
                              -- , ("Q00788","2JX4") -- N intracellular loop; too short
                              -- , ("P32241","1OGT") -- N extracellular chain fragment
                              -- , ("P41587","2X57") -- N Extracellular domain, fragment
                              ]


data Region = ExtraCellular | Transmembrane | Cellular deriving (Ord, Eq, Show)

residueRegions :: Map UniprotID (Map Region [Int])
residueRegions = M.fromList
                 [ ("P29274", M.fromList
                    [ (ExtraCellular, concat [[1..9],[67..75],[99..120],[144..178],[253..266]])
                    , (Transmembrane, concat [[10..32],[44..66],[76..98],[121..143],[279..201],[230..252],[267..289]])
                    , (Cellular    , concat [[33..43],[99..120],[202..229],[290..412]])
                    ]
                   )
                 , ("P07550", M.fromList
                    [ (ExtraCellular, concat [[1..35],[93..106],[170..200],[298..306]])
                    , (Transmembrane, concat [[36..58],[70..92],[107..129],[150..169],[201..223],[275..297],[207..326]])
                    , (Cellular     , concat [[59..69],[130..149],[224..274],[327..413]])
                    ]
                   )
                 , ("P34998", M.fromList
                    [ (ExtraCellular, concat [[1..119],[205..231],[279..297],[364..372]])
                    , (Transmembrane, concat [[120..142],[182..204],[232..251],[258..278],[298..320],[341..363],[373..395]])
                    , (Cellular     , concat [[143..181],[252..257],[321..340],[396..444]])
                    ]
                   )
                 , ("Q60748", M.fromList
                    [ (ExtraCellular, concat [[1..134],[193..201],[266..284],[351..359]])
                    , (Transmembrane, concat [[135..157],[170..192],[202..224],[245..265],[285..307],[328..350],[360..382]])
                    , (Cellular     , concat [[225..244],[308..327],[383..431],[193..201],[266..284],[351..359]])
                    ]
                   )
                 , ("P23945", M.fromList
                    [ (ExtraCellular, concat [[1..367],[422..443],[509..531],[598..606]])
                    , (Transmembrane, concat [[368..387],[399..421],[444..466],[486..508],[532..554],[575..597],[607..629]])
                    , (Cellular     , concat [[388..398],[467..485],[555..574],[630..695]])
                    ]
                   )
                 , ("P41586", M.fromList
                    [ (ExtraCellular, concat [[1..155],[211..229],[288..306],[372..380]])
                    , (Transmembrane, concat [[156..178],[191..210],[230..252],[265..287],[307..329],[349..371],[381..403]])
                    , (Cellular     , concat [[179..190],[253..264],[330..348],[404..468]])
                    ]
                   )
                 , ("Q00788", M.fromList
                    [ (ExtraCellular, concat [[1..40],[99..117],[179..205],[297..305]])
                    , (Transmembrane, concat [[41..63],[76..98],[118..135],[156..178],[206..228],[274..296],[306..328]])
                    , (Cellular     , concat [[64..75],[136..155],[229..273],[329..371]])
                    ]
                   )
                 , ("P32241", M.fromList
                    [ (ExtraCellular, concat [[1..145],[201..219],[277..295],[361..369]])
                    , (Transmembrane, concat [[146..168],[181..200],[220..242],[254..276],[296..318],[338..360],[370..392]])
                    , (Cellular     , concat [[169..180],[243..253],[319..337],[393..457]])
                    ]
                   )
                 , ("P41587", M.fromList
                    [ (ExtraCellular, concat [[1..128],[179..205],[263..280],[353..356]])
                    , (Transmembrane, concat [[129..151],[160..178],[206..228],[240..262],[281..303],[330..352],[357..379]])
                    , (Cellular     , concat [[152..159],[229..239],[304..329],[380..438]])
                    ]
                   )
                 ]

retrievePDB :: Verbosity -> FilePath -> PDBID -> IO FilePath
retrievePDB verbose dir pdb = retrieveURL
                                verbose
                                (printf "http://www.pdb.org/pdb/files/%s.pdb" pdb)
                                (dir </> pdb <.> "pdb")

retrievePDBs :: Verbosity -> FilePath -> [PDBID] -> IO [FilePath]
retrievePDBs v dir = pMapM (retrievePDB v dir)


clean :: Execute ()
clean = do
  modelsdir          <- get modbase_model_root <$> ask
  tmscoreSummaryFile <- get (tmScoreOutputSummary . tmScoreParams) <$> ask
  tmscoreResults     <- get (tmScoreOutputDir . tmScoreParams) <$> ask

  let toremove = [modelsdir, tmscoreSummaryFile, tmscoreResults]

  writeLog stdout "modbase_retrieve_model" $ printf "Recursively removing %s\n" (intercalate ", " toremove)
  liftIO $ mapM_ removeDirectoryRecursive toremove




options :: [OptDescr (Params -> Params)]
options = [ Option "f" ["force"] (NoArg (\ps -> set force True ps)) "Overwrite old files. Default=False"
          , Option "v" ["verbose"] (NoArg (\ps -> set verbose True ps)) "Default=False"
          , Option "u" ["uniprot-interactors"]
                       (OptArg (\prot ps -> set gprotein (fromMaybe (get gprotein ps) prot) ps) "G protein name. Default=gs")
                       "Get the Uniprot IDs for the interactors of this GProtein from PRED-COUPLE's training set"
          , Option "b" ["browser"]
                       (ReqArg (\path ps -> set browser path ps) "PATH")
                       "Use this command to open a browser. Default=chromium"
          , Option "l" ["limit-model-pdbs"]
                       (OptArg (\lim ps -> set limit_model_pdbs (fromMaybe (get limit_model_pdbs ps) (read<$>lim)) ps) "Int")
                       "Limit the number of pdbs extracted to this number"
          , Option "M" ["local-modbase-root"]
                       (OptArg (\path ps -> set modbase_model_root (fromMaybe (get modbase_model_root ps) path) ps) "DIR")
                       "Use this directory to put the modbase models. Default=modbase-models"
          ]
 

readParams :: [String] -> Either [String] Params
readParams args = case getOpt Permute options args of
                    (o,n,[]) -> Right $ foldl (flip id) def o
                    (_,_,es) -> Left es


chooseCmd :: String -> Params -> IO ()
chooseCmd cmd ps =
    let run = exec ps
    in case cmd of
         "ids" -> run predcouple_uniprot_ids >>= mapM_ putStrLn
         "modbase-retrieve" -> run search_modbase
         "modbase-write" -> run get_and_write_modbase_pdbs >> return ()
         "tmscore" -> run allWorstModels >>= putStrLn . table
         "clean" -> run clean


usage es = ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: CMD [OPTION...] where CMD = ids | modbase-retrieve | modbase-write | tmscore | clean"


handleCLI (cmd:args) = let params = readParams args
                       in case params of
                            Right ps -> chooseCmd cmd ps
                            Left es -> usage es
handleCLI r = usage r

    




-- handle :: [String] -> IO ()
-- handle args =
--     case args of
--       ["ids"]                                     -> exec' predcouple_uniprot_ids >>= mapM_ putStrLn
--       ["modbase", "search"]                       -> exec' search_modbase
--       ["modbase", "retrieve"]                     -> exec' modbase_retrieve_models >> return ()
--       ["modbase", "retrieve", force]              -> exec
--                                                        with { _force = read force }
--                                                        modbase_retrieve_models >> return ()
--       ["modbase", "retrieve", force, count]       -> exec
--                                                        with { _force = read force, _limit_models = read count }
--                                                        modbase_retrieve_models >> return ()
--       ["modbase", "write", force, "all"]          -> exec'
--                                                        get_and_write_modbase_pdbs >> return ()

--       ["modbase", "write", force, mcount, pcount] -> exec
--                                                        with { _limit_models = read mcount, _limit_model_pdbs = read pcount }
--                                                        get_and_write_modbase_pdbs >> return ()

--       ["tmscore"]                                 -> exec' allWorstModels >>= putStrLn . table
--       ("tmscore":uniprotids)                      -> exec' (pMapM worstModel uniprotids) >>= putStrLn . table . catMaybes

--       ["best-tmscore"]                            -> exec' allBestModels >>= putStrLn . table
--       ("best-tmscore":uniprotids)                 -> exec' (pMapM bestModel uniprotids) >>= putStrLn . table . catMaybes

--       ["clean"]                                   -> exec' clean
--       ["clean", dir]                              -> exec with { _modbase_model_root = dir } clean

--       ["pdbs", v]                                 -> do retrievePDBs (read v) "/tmp/pdbs" . M.elems $ experimentalPDBs
--                                                         return ()


main = getArgs >>= withLogger loggerchan . handleCLI

