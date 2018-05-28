{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module NanoML.Features
  ( Feature
  , TypeMap
  , spanToTuple
  , fsNormalCtx
  , fTypeCtx
  , fSizeCtx
  , noCtx
  , toOneHot
  , boolToDouble
  ) where

import Data.Bifunctor
import Data.Default
import Data.Typeable
import Data.Data
import qualified Data.HashMap.Strict as HashMap

import NanoML.Classify

import Language.Python.Common

import Debug.Trace

type Feature a = ([String], (ES -> ES -> [a]), [a])
type TypeMap = (HashMap.HashMap String String, HashMap.HashMap (Int, Int, Int, Int) String)

instance Default Bool where
  def = False

mkContextLabels :: String -> [String]
mkContextLabels l = [l, l++"-P", l++"-C1", l++"-C2", l++"-C3"]

mkContextFeatures :: (Default a) => (ES -> a) -> ES -> ES -> [a]
mkContextFeatures mkF p e =
  [mkF e, mkF p] ++ take 3 (map mkF (subESes' e) ++ repeat def)

noCtx :: Feature a -> Feature a
noCtx = fst3 (take 1) . snd3 (fmap (fmap (take 1)))

fst3 :: (a -> z) -> (a,b,c) -> (z,b,c)
fst3 f (a, b, c) = (f a, b, c)
snd3 :: (b -> z) -> (a,b,c) -> (a,z,c)
snd3 f (a, b, c) = (a, f b, c)
thd3 :: (c -> z) -> (a,b,c) -> (a,b,z)
thd3 f (a, b, c) = (a, b, f c)

noCtx' :: (b -> Feature a) -> (b -> Feature a)
noCtx' = (.) noCtx

collapseStmtExpr :: (ES -> a) -> (ES -> a)
collapseStmtExpr f (St (StmtExpr e _)) = f (Ex e)
collapseStmtExpr f x = f x

getExprConstr :: ES -> Maybe Constr
getExprConstr = collapseStmtExpr getExprConstr'

getExprConstr' (Ex e) = Just $ toConstr e
getExprConstr' (St s) = Nothing

getStmtConstr :: ES -> Maybe Constr
getStmtConstr (Ex _) = Nothing
getStmtConstr (St s) = Just $ toConstr s

getOpConstr :: ES -> Maybe Constr
getOpConstr = collapseStmtExpr getOpConstr'

getOpConstr' (Ex (BinaryOp op _ _ _)) = Just $ toConstr op
getOpConstr' (Ex (UnaryOp op _ _))    = Just $ toConstr op
getOpConstr' _                        = Nothing

-- primitive types and OBJECT types
slicerTypes :: [String]
slicerTypes = ["LIST", "TUPLE", "SET", "DICT", "INSTANCE", "CLASS", "FUNCTION",
  "module", "int", "long", "float", "str", "unicode", "bool", "NoneType"]

getType :: TypeMap -> ES -> Maybe String
getType m = collapseStmtExpr (getType' m)

getType' _ (St _) = Nothing
getType' (mVar, mSpan) (Ex e) = Just $ case HashMap.lookup (spanToTuple $ annot e) mSpan of
  Just t' | elem t' slicerTypes -> t'
  Just t' | otherwise -> "other"
  Nothing -> case e of
    Var (Ident x _) _ -> case HashMap.lookup x mVar of
      Just t' | elem t' slicerTypes -> t'
      Just t' | otherwise -> "other"
      Nothing -> "unknown"
    _ -> "unknown"

getSize :: ES -> Int
getSize x = 1 + (sum $ getSize <$> subESes' x)

spanToTuple :: SrcSpan -> (Int, Int, Int, Int)
spanToTuple (SpanCoLinear _ r c1 c2) = (r, c1, r, c2)
spanToTuple (SpanMultiLine _ r1 c1 r2 c2) = (r1, c1, r2, c2)
spanToTuple (SpanPoint _ r c) = (r, c, r, c)
spanToTuple SpanEmpty = error "empty span"

fTypeCtx :: TypeMap -> Feature (Maybe String)
fTypeCtx tm = ( mkContextLabels ("Type"), mkContextFeatures (getType tm), Nothing : (Just <$> (slicerTypes ++ ["other, unknkown"])) )

fStmtConstrCtx :: Feature (Maybe Constr)
fStmtConstrCtx = ( mkContextLabels "Stmt-Constr", mkContextFeatures getStmtConstr, Nothing : (Just <$> dataTypeConstrs (dataTypeOf (Pass ()))) )

fExprConstrCtx :: Feature (Maybe Constr)
fExprConstrCtx = ( mkContextLabels "Expr-Constr", mkContextFeatures getExprConstr, Nothing : (Just <$> dataTypeConstrs (dataTypeOf (None ()))) )

fOpConstrCtx :: Feature (Maybe Constr)
fOpConstrCtx = ( mkContextLabels "Op-Contr", mkContextFeatures getOpConstr, Nothing : (Just <$> dataTypeConstrs (dataTypeOf (And ()))) )

fSizeCtx :: Feature (Maybe String)
fSizeCtx = ( mkContextLabels "Size", mkContextFeatures (Just . show . getSize), (Just . show) <$> [1..])

-- fsNormalCtx :: [Feature (Maybe Constr)]
-- fsNormalCtx = [fStmtConstrCtx, fExprConstrCtx, fOpConstrCtx]
fsNormalCtx :: [Feature (Maybe String)]
fsNormalCtx = fmap (fmap' (fmap showConstr)) [fStmtConstrCtx, fExprConstrCtx, fOpConstrCtx]

fmap' :: (a -> b) -> Feature a -> Feature b
fmap' f (ls, feat, options) = (ls, feat', f <$> options)
  where
    feat' x y = f <$> feat x y

toOneHot :: (Show a, Eq a) => Feature a -> [Feature (Maybe String)]
toOneHot fs@(names, f, options) = (fmap' (Just . show . boolToDouble)) <$> (toOneHot' fs <$> options)

boolToDouble :: Bool -> Double
boolToDouble b = if b then 1.0 else 0.0

toOneHot' :: (Show a, Eq a) => Feature a -> a -> Feature Bool
toOneHot' (names, f, options) x = (names', f', [True, False])
  where
    names' = (++ "-Is-"++(show x)) <$> names
    f' a b = (== x) <$> f a b
