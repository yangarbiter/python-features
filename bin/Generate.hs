{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Exception (assert)
import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..), eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Algorithm.Diff as Diff
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Csv
import Data.Data (Constr, showConstr)
import Data.Either
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import GHC.Generics
import Options.Generic hiding (All(..))
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.Printf

import NanoML.Classify
import NanoML.ExampleFeatures

import qualified Language.Python.Version2.Parser as Py2
import qualified Language.Python.Version3.Parser as Py3
import Language.Python.Common

import Debug.Trace


type Prog = [StatementSpan]
type MySpan = (Int, Int, Int, Int)
type ErrorSlice = [MySpan]

data Generate = Generate
  { source :: FilePath
  , features :: String
  , out :: FilePath
  }
  deriving (Generic, Show)
instance ParseRecord Generate

main :: IO ()
main = do
  Generate {source=src, features=cls, out=out} <-
    getRecord "generate-features"
  jsons <- lines <$> readFile src
  case cls of
    "op"
      -> mkBadFeatures out cls (requestTypeMap (noCtx <$> fsNormalCtx) (noCtx . fTypeCtx)) jsons
    "op+slice"
      -> mkBadFeaturesWithSlice All out cls (requestTypeMap (noCtx <$> fsNormalCtx) (noCtx . fTypeCtx)) jsons
    "op+context"
      -> mkBadFeatures out cls (requestTypeMap fsNormalCtx fTypeCtx) jsons
    "op+context+slice"
      -> mkBadFeaturesWithSlice All out cls (requestTypeMap fsNormalCtx fTypeCtx) jsons
    -- "op+context+size"
    --   -> mkBadFeatures out cls (preds_tsize ++ preds_tis ++ map only_ctx preds_tis_ctx) jsons
    -- "op+size"
    --   -> mkBadFeatures out cls (preds_tsize ++ preds_tis) jsons

requestTypeMap :: [Feature a] -> (TypeMap -> Feature a) -> TypeMap -> [Feature a]
requestTypeMap fs tf tm = fs ++ [tf tm]

data WithSlice = JustSlice | All deriving Eq

mkBadFeatures :: (ToField a) => String -> String -> (TypeMap -> [Feature a]) -> [String] -> IO ()
mkBadFeatures = mkBadFeaturesWithSlice JustSlice

mkBadFeaturesWithSlice :: (ToField a) => WithSlice -> String -> String -> (TypeMap -> [Feature a]) -> [String] -> IO ()
mkBadFeaturesWithSlice withSlice out nm fs jsons = do
  let uniqs = concatMap mkDiffs jsons
  let feats = [ ((h, f'), (ss, bad, fix, exceptionSpan, slice, all, idx))
              | (ss, p, bad, fix, exceptionSpan, slice, badTypes, idx) <- uniqs
              , (h, f) <- maybeToList $ runTFeaturesDiff slice exceptionSpan (fs badTypes) (ss,p)
              , let f' = filter (\r -> withSlice == All || r HashMap.! "F-InSlice" == "1.0") f
                -- a one-constraint core is bogus, this should be impossible
              -- , length f' > 1
              , let all = nub $ map getSpan (concatMap allSubESes $ St <$> p)
              ]
  --let feats' = filter (\(_, (_,_,_,cs,_,_)) -> not (null cs)) feats
  let mkMean f xs = sum (map f xs) / genericLength xs
  let mkFrac (_, (ss, _, _, _, _, all, _)) = genericLength ss / genericLength all
  -- For discarding outliers by fraction of error slice that changed rather than
  -- whole program. Doesn't seem to make a huge difference overall.
  -- let mkFrac (_, (ss, _, _, cs, _all, _)) = genericLength (ss `intersect` cs) / genericLength cs
  let mean = mkMean mkFrac feats :: Double
  let std  = sqrt $ mkMean (\x -> (mkFrac x - mean) ^ 2) feats
  forM_ feats $ \ f@((header, features), (ss, bad, fix, _, cs, allspans, i)) -> do
    if
      | mkFrac f > mean+std -> do
          -- printf "OUTLIER: %.2f > %.2f\n" (mkFrac f :: Double) (mean+std)
          return ()
      | null ss -> do
        -- putStrLn "NO DIFF"
        -- putStrLn bad
        -- putStrLn "---------------------------"
        -- putStrLn fix
        return ()
      | otherwise -> do
        let fn = printf "%04d" (i :: Int)
        let path = out </> nm </> fn <.> "csv"
        createDirectoryIfMissing True (takeDirectory path)
        LBSC.writeFile path $ encodeByName header features
        let path = out </> fn <.> "ml"
        writeFile path $ unlines $ [ bad, "", "(* fix", fix, "*)", ""
                                   , "(* changed spans" ] ++ map show (spanToTuple <$> ss) ++ [ "*)" ]
                                ++ [ "", "(* error slice" ] ++ map show cs ++ [ "*)" ]
                                ++ [ "", "(* all spans" ] ++ map show allspans ++ [ "*)" ]

    -- let (header, features) = unzip $ map (runTFeaturesDiff fs) uniqs
    -- let path = "data/" ++ nm ++ ".csv"
    -- LBSC.writeFile path $ encodeByName (head header) (concat features)
  printf "MEAN / STD frac: %.3f / %.3f\n" mean std


parseTopForm :: Int -> String -> Either String [StatementSpan]
parseTopForm v code = case (parseModule v) code "foo.py" of
  Left e -> Left (show e)
  Right (Module decls, _) -> Right decls

parseModule 2 = Py2.parseModule
parseModule 3 = Py3.parseModule

mkDiffs :: String -> [([SrcSpan], Prog, String, String, MySpan, ErrorSlice, TypeMap, Int)]
mkDiffs json = case eitherDecode (LBSC.pack json) of
  Left e -> {-trace e-} error "e1"
    -- -> HashSet.fromList . maybeToList $ mkDiff fix bad
  Right (MkInSample bad' fix' v _ _ _ _ _)
  --Right (MkInSample bads' (fix':_))
    | Left e <- parseTopForm v fix'
    -> {-trace e-} error "e2"
  Right (MkInSample bad' fix' v _ _ _ _ _)
  --Right (MkInSample bads' (fix':_))
    | Left e <- parseTopForm v bad'
    -> {-trace e-} error "e3"
  Right (MkInSample bad' fix' v exceptionSpan spanSlice idx varTypes spanTypes)
  --Right (MkInSample bads' (fix':_))
    | Right fix <- parseTopForm v fix'
    , Right bad <- parseTopForm v bad'
    , let ss = mkDiff'' bad fix
    -- , not (null ss)
    -- -> maybeToList . fmap (,bad, bad', fix') $ mkDiff' bad' fix'
    -> [(ss, bad, bad', fix', listToTuple exceptionSpan, listToTuple <$> spanSlice, foo (varTypes, spanTypes), idx)]

  -- _ -> mempty
  v -> error (show v)

foo :: (HashMap.HashMap String String, HashMap.HashMap String String) -> TypeMap
foo (varTypes, spanTypes) = (varTypes, spanTypes')
  where
    spanTypes' = HashMap.fromList [(read k, v) | (k, v) <- HashMap.toList spanTypes]

listToTuple :: [Int] -> (Int, Int, Int, Int)
listToTuple [a,b,c,d] = (a,b,c,d)

mkDiff'' :: Prog -> Prog -> [SrcSpan] --TODO currently returns []?
mkDiff'' bad fix
  --- | null x
  -- = trace (render $ prettyProg bad) $ trace (render $ prettyProg fix) $ trace "" $ undefined
  | otherwise
  = assert (not (null x)) $ x
  where
  -- x = Set.toList (diffSpans (collapseDiff (getDiff $ diffExprsT bs fs)))
  x = Set.toList (diffSpans (getDiff $ diffExprsT (St <$> bad) (St <$> fix)) (St <$> bad))

runTFeaturesDiff
  :: (ToField a) => ErrorSlice -> MySpan -> [Feature a] -> ([SrcSpan], Prog)
  -> Maybe (Header, [NamedRecord])
runTFeaturesDiff slice exceptionSpan fs (ls, bad)
  | null samples
  = error "why Nothing"
  | otherwise
  = Just (header, samples)
  where
  header = Vector.fromList
         $ ["SourceSpan", "L-NoChange", "L-DidChange", "F-InSlice"]
        ++ concatMap (\(ls,_) -> map mkFeature ls) fs
        ++ ["F-PythonBlame"]

  samples = concatMap mkTypeOut bad

  didChange l
    | any (l ==) ls
    = ["L-DidChange" .= (1::Double), "L-NoChange" .= (0::Double)]
    | otherwise
    = ["L-DidChange" .= (0::Double), "L-NoChange" .= (1::Double)]

  mkTypeOut :: StatementSpan -> [NamedRecord]
  mkTypeOut te = ctfold f [] (St te)
    where
    f p e acc = (:acc) . namedRecord $
                ["SourceSpan" .= show (spanToTuple $ getSpan e)]
             ++ didChange (getSpan e)
             ++ inSlice e slice
             ++ concatMap (\(ls,c) -> zipWith (.=) (map mkFeature ls) (c p e)) fs
             ++ pythonBlame e exceptionSpan

inSlice e slice = ["F-InSlice" .= inSlice' e slice]
pythonBlame e exceptionSpan = ["F-PythonBlame" .= inSlice' e [exceptionSpan]]

inSlice' :: ES -> ErrorSlice -> Bool
inSlice' e spanSlice = elem (spanToTuple $ getSpan e) spanSlice

mkLabel :: String -> BSC.ByteString
mkLabel s = BSC.pack ("L-" ++ s)

mkFeature :: String -> BSC.ByteString
mkFeature s = BSC.pack ("F-" ++ s)

data InSample = MkInSample { bad :: String,
                             fix :: String,
                             pyVersion :: Int,
                             exceptionSpan :: [Int],
                             spanSlice :: [[Int]], --[(Int, Int, Int, Int)],
                             index :: Int,
                             varTypes :: HashMap.HashMap String String,
                             spanTypes :: HashMap.HashMap String String
                           }
  deriving (Show, Generic)
instance FromJSON InSample

instance ToField Constr where
  toField = toField . showConstr
instance ToField Bool where
  toField = toField . show
