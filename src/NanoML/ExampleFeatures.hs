module NanoML.ExampleFeatures
  ( preds_tis
  , preds_tis_ctx
  , type_tis
  , only_ctx
  , Feature
  , TypeMap
  , spanToTuple
  , boolToDouble
  ) where

import Data.Bifunctor
import Data.Typeable
import Data.Data
import qualified Data.HashMap.Strict as HashMap

import NanoML.Classify

import Language.Python.Common

import Debug.Trace

type Feature = ([String], (ES -> ES -> [Double]))
type TypeMap = (HashMap.HashMap String String, HashMap.HashMap (Int, Int, Int, Int) String)

mkContextLabels :: String -> [String]
mkContextLabels l = [l, l++"-P", l++"-C1", l++"-C2", l++"-C3"]

mkContextFeatures :: (ES -> Double) -> ES -> ES -> [Double]
mkContextFeatures mkF p e =
  [mkF e, mkF p] ++ take 3 (map mkF (subESes e) ++ repeat 0)

only_ctx :: Feature -> Feature
only_ctx = first (drop 1) . second (fmap (fmap (drop 1)))

preds_tis :: [Feature]
preds_tis = map (first (take 1) . second (fmap (fmap (take 1))))
            preds_tis_ctx

type_tis :: TypeMap -> [Feature]
type_tis tm = map (first (take 1) . second (fmap (fmap (take 1))))
              (type_tis_ctx tm)
type_tis_ctx :: TypeMap -> [Feature]
type_tis_ctx tm = ($ tm) <$> tis_type_ctx <$> slicerTypes

preds_tis_ctx :: [Feature]
preds_tis_ctx = tis_kind_ctx_list <$> constructors

eisToTis :: (ExprSpan -> Double) -> ES -> Double
eisToTis _ (St _) = 0
eisToTis f (Ex e) = f e


exprConstructors = dataTypeConstrs (dataTypeOf ((None ()) :: Expr ()))
stmtConstructors = dataTypeConstrs (dataTypeOf ((Pass ()) :: Statement ()))
constructors = exprConstructors ++ stmtConstructors

boolToDouble :: Bool -> Double
boolToDouble b = if b then 1 else 0

eis_kind :: Constr -> ES -> Double
eis_kind c (Ex e) = boolToDouble $ c == toConstr e
eis_kind c (St s) = boolToDouble $ c == toConstr s

kindToString :: ES -> String
kindToString (Ex e) = showConstr $ toConstr e

-- primitive types and OBJECT types
slicerTypes :: [String]
slicerTypes = ["LIST", "TUPLE", "SET", "DICT", "INSTANCE", "CLASS", "FUNCTION",
  "module", "int", "long", "float", "str", "unicode", "bool", "NoneType"]

eis_type :: String -> TypeMap -> ExprSpan -> Double
eis_type t (mVar, mSpan) e = case HashMap.lookup (spanToTuple $ annot e) mSpan of
  Just t' | t' == t -> 1
  Just t' | t' /= t -> 0
  Nothing -> case e of
    Var (Ident x _) _ -> case HashMap.lookup x mVar of
      Just t' | t' == t -> 1
      _ -> 0
    _ -> 0

spanToTuple :: SrcSpan -> (Int, Int, Int, Int)
spanToTuple (SpanCoLinear _ r c1 c2) = (r, c1, r, c2)
spanToTuple (SpanMultiLine _ r1 c1 r2 c2) = (r1, c1, r2, c2)
spanToTuple (SpanPoint _ r c) = (r, c, r, c)
spanToTuple SpanEmpty = error "empty span"

tis_type_ctx :: String -> TypeMap -> Feature
tis_type_ctx t tm = ( mkContextLabels ("Is-T-"++t), mkContextFeatures (eisToTis (eis_type t tm)) )

tis_kind_ctx_list :: Constr -> Feature
tis_kind_ctx_list c = ( mkContextLabels ("Is-"++(showConstr c)), mkContextFeatures (eis_kind c) )
