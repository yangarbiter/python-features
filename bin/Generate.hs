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
type ErrorSlice = [Int] --Temporary version uses list of line numbers

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
      -> mkBadFeatures out cls (preds_tis) jsons
    "op+slice"
      -> mkBadFeaturesWithSlice All out cls (preds_tis) jsons
    "op+context"
      -> mkBadFeatures out cls (preds_tis ++ map only_ctx preds_tis_ctx) jsons
    -- "op+context+size"
    --   -> mkBadFeatures out cls (preds_tsize ++ preds_tis ++ map only_ctx preds_tis_ctx) jsons
    -- "op+size"
    --   -> mkBadFeatures out cls (preds_tsize ++ preds_tis) jsons

data WithSlice = JustSlice | All deriving Eq

mkBadFeatures :: String -> String -> [Feature] -> [String] -> IO ()
mkBadFeatures = mkBadFeaturesWithSlice JustSlice

mkBadFeaturesWithSlice :: WithSlice -> String -> String -> [Feature] -> [String] -> IO ()
mkBadFeaturesWithSlice withSlice out nm fs jsons = do
  let uniqs = concatMap mkDiffs jsons
  let feats = [ ((h, f'), (ss, bad, fix, c, all, idx))
              | (ss, p, bad, fix, slice, idx) <- uniqs
              , (h, f, c) <- maybeToList $ runTFeaturesDiff slice fs (ss,p)
              , let f' = filter (\r -> withSlice == All || r HashMap.! "F-InSlice" == "1.0") f
                -- a one-constraint core is bogus, this should be impossible
              -- , length f' > 1
              , let all = nub $ map getSrcSpan (concatMap allSubESes $ St <$> p)
              ]
  --let feats' = filter (\(_, (_,_,_,cs,_,_)) -> not (null cs)) feats
  let mkMean f xs = sum (map f xs) / genericLength xs
  let mkFrac (_, (ss, _, _, _, all, _)) = genericLength ss / genericLength all
  -- For discarding outliers by fraction of type error slice that changed rather than
  -- whole program. Doesn't seem to make a huge difference overall.
  -- let mkFrac (_, (ss, _, _, cs, _all, _)) = genericLength (ss `intersect` cs) / genericLength cs
  let mean = mkMean mkFrac feats :: Double
  let std  = sqrt $ mkMean (\x -> (mkFrac x - mean) ^ 2) feats
  forM_ feats $ \ f@((header, features), (ss, bad, fix, cs, allspans, i)) -> do
    if
      | mkFrac f > mean+std -> do
          printf "OUTLIER: %.2f > %.2f\n" (mkFrac f :: Double) (mean+std)
      -- | null ss -> do
      --   putStrLn "NO DIFF"
      --   putStrLn bad
      --   putStrLn "---------------------------"
      --   putStrLn fix
      | otherwise -> do
        let fn = printf "%04d" (i :: Int)
        let path = out </> nm </> fn <.> "csv"
        createDirectoryIfMissing True (takeDirectory path)
        LBSC.writeFile path $ encodeByName header features
        let path = out </> fn <.> "ml"
        writeFile path $ unlines $ [ bad, "", "(* fix", fix, "*)", ""
                                   , "(* changed spans" ] ++ map show ss ++ [ "*)" ]
                                ++ [ "", "(* type error slice" ] ++ map show cs ++ [ "*)" ]
                                ++ [ "", "(* all spans" ] ++ map show allspans ++ [ "*)" ]

    -- let (header, features) = unzip $ map (runTFeaturesDiff fs) uniqs
    -- let path = "data/" ++ nm ++ ".csv"
    -- LBSC.writeFile path $ encodeByName (head header) (concat features)
  printf "MEAN / STD frac: %.3f / %.3f\n" mean std

mkFixFeatures :: String -> [Feature] -> [String] -> IO ()
mkFixFeatures nm fs jsons = do
  let fixes = concatMap mkFixes jsons
  let (header, features) = unzip $ map (runTFeaturesTypes fs) fixes
  let path = "data/" ++ nm ++ ".csv"
  LBSC.writeFile path $ encodeByName (head header) (concat features)


traceStats :: [[NamedRecord]] -> IO ()
traceStats outss = do
  let fracs = [ fromIntegral (length (filter (\out -> out HashMap.! "L-DidChange" == "1.0") outs)) / fromIntegral (length outs)
              | outs <- outss
              , length outs > 0
              ] :: [Double]
  hPrintf stderr "%s\n" (show fracs)
  hPrintf stderr "Min: %f\n" (minimum fracs)
  hPrintf stderr "Max: %f\n" (maximum fracs)
  let avg = sum fracs / fromIntegral (length fracs)
  hPrintf stderr "Avg: %f\n" avg
  let std = sqrt (sum [(f - avg) ^ 2 | f <- fracs] / fromIntegral (length fracs))
  hPrintf stderr "Std: %f\n" std
  return ()

-- uniqDiffs :: [String] -> HashSet ([SrcSpan], Prog, String, String, Int)
-- uniqDiffs = foldl' (\seen json -> seen `mappend` HashSet.fromList (mkDiffs json)) mempty

parseTopForm :: Int -> String -> Either String [StatementSpan]
parseTopForm v code = case (parseModule v) code "foo.py" of
  Left e -> Left (show e)
  Right (Module decls, _) -> Right decls

parseModule 2 = Py2.parseModule
parseModule 3 = Py3.parseModule

mkDiffs :: String -> [([SrcSpan], Prog, String, String, ErrorSlice, Int)]
mkDiffs json = case eitherDecode (LBSC.pack json) of
  Left e -> {-trace e-} error "e1"
    -- -> HashSet.fromList . maybeToList $ mkDiff fix bad
  Right (MkInSample bad' fix' v _ _)
  --Right (MkInSample bads' (fix':_))
    | Left e <- parseTopForm v fix'
    -> {-trace e-} error "e2"
  Right (MkInSample bad' fix' v _ _)
  --Right (MkInSample bads' (fix':_))
    | Left e <- parseTopForm v bad'
    -> {-trace e-} error "e3"
  Right (MkInSample bad' fix' v slice idx)
  --Right (MkInSample bads' (fix':_))
    | Right fix <- parseTopForm v fix'
    , Right bad <- parseTopForm v bad'
    , let ss = mkDiff'' bad fix
    -- , not (null ss)
    -- -> maybeToList . fmap (,bad, bad', fix') $ mkDiff' bad' fix'
    -> [(ss, bad, bad', fix', slice, idx)]

  -- _ -> mempty
  v -> error (show v)

mkProgs :: String -> Either String (Prog, Prog)
mkProgs json = case eitherDecode (LBSC.pack json) of
  Left e -> {-trace e-} error "bad json"
  Right (MkInSample bad' fix' v _ _)
    | Left e <- parseTopForm v fix'
    -> {-trace e-} error "fix no parse"
  Right (MkInSample bad' fix' v _ _)
    | Left e <- parseTopForm v bad'
    -> {-trace e-} error "bad no parse"
  Right (MkInSample bad' fix' v _ _)
    | Right fix <- parseTopForm v fix'
    , Right bad <- parseTopForm v bad'
    -> Right (bad, fix) -- [(ss, bad, bad', fix')]

mkFixes :: String -> [Prog]
mkFixes json = case eitherDecode (LBSC.pack json) of
  Left e -> {-trace e-} mempty
  Right (MkInSample bad' fix' v _ _)
  --Right (MkInSample bads' (fix':_))
    | Right fix <- parseTopForm v fix'
    -> [fix]
    -- -> HashSet.fromList . maybeToList $ mkDiff fix bad
  Right (MkInSample bad' fix' v _ _)
  --Right (MkInSample bads' (fix':_))
    | Left e <- parseTopForm v fix'
    -> {-trace e-} error "f1"
  Right (MkInSample bad' fix' v _ _)
  --Right (MkInSample bads' (fix':_))
    | Left e <- parseTopForm v bad'
    -> {-trace e-} error "f2"
  x -> error (show x)

mkDiff'' :: Prog -> Prog -> [SrcSpan] --TODO currently returns []
mkDiff'' bad fix
  --- | null x
  -- = trace (render $ prettyProg bad) $ trace (render $ prettyProg fix) $ trace "" $ undefined
  | otherwise
  = assert (not (null x)) $ x
  where
  -- x = Set.toList (diffSpans (collapseDiff (getDiff $ diffExprsT bs fs)))
  x = Set.toList (diffSpans (getDiff $ diffExprsT (St <$> bad) (St <$> fix)) (St <$> bad))

runTFeaturesDiff
  :: ErrorSlice -> [Feature] -> ([SrcSpan], Prog)
  -> Maybe (Header, [NamedRecord], [SrcSpan])
runTFeaturesDiff slice fs (ls, bad)
  | null samples
  = error "why Nothing"
  | otherwise
  = Just (header, samples, [])
  where
  header = Vector.fromList
         $ ["SourceSpan", "L-NoChange", "L-DidChange", "F-InSlice"]
        ++ concatMap (\(ls,_) -> map mkFeature ls) fs

  samples = concatMap mkTypeOut bad

  didChange l
    | any (l ==) ls
    = ["L-DidChange" .= (1::Double), "L-NoChange" .= (0::Double)]
    | otherwise
    = ["L-DidChange" .= (0::Double), "L-NoChange" .= (1::Double)]

  inSlice l = ["F-InSlice" .= (x::Double)]
    where
      x = boolToDouble $ any (spanOnLine l) slice

  mkTypeOut :: StatementSpan -> [NamedRecord]
  mkTypeOut te = ctfold f [] (St te)
    where
    f p e acc = (:acc) . namedRecord $
                ["SourceSpan" .= show (getSrcSpan e)]
             ++ didChange (getSrcSpan e)
             ++ inSlice (getSrcSpan e)
             ++ concatMap (\(ls,c) -> zipWith (.=) (map mkFeature ls) (c p e)) fs

spanOnLine :: SrcSpan -> Int -> Bool
spanOnLine (SpanCoLinear _ x _ _) y = x == y
spanOnLine (SpanMultiLine _ x1 _ x2 _) y = x1 <= y && y <= x2
spanOnLine (SpanPoint _ x _) y = x == y
spanOnLine SpanEmpty y = error "empty span"

boolToDouble :: Bool -> Double
boolToDouble True = 1
boolToDouble False = 0

runTFeaturesTypes
  :: [Feature] -> Prog
  -> (Header, [NamedRecord])
runTFeaturesTypes fs fix = (header, samples)
  where
  header = Vector.fromList
         $ concatMap (\(ls,_) -> map mkFeature ls) fs

  samples = concatMap mkTypeOut fix

  mkTypeOut :: StatementSpan -> [NamedRecord]
  mkTypeOut te = ctfold f [] (St te)
    where
    f p e acc = (:acc) . namedRecord $
                concatMap (\(ls,c) -> zipWith (.=) (map mkFeature ls) (c p e)) fs

mkLabel :: String -> BSC.ByteString
mkLabel s = BSC.pack ("L-" ++ s)

mkFeature :: String -> BSC.ByteString
mkFeature s = BSC.pack ("F-" ++ s)


data InSample = MkInSample { bad :: String, fix :: String, pyVersion :: Int, slice :: ErrorSlice, index :: Int}
  deriving (Show, Generic)
instance FromJSON InSample
