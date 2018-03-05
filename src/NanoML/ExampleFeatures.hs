module NanoML.ExampleFeatures
  ( preds_tis
  , preds_tis_ctx
  , type_tis
  , only_ctx
  , Feature
  , TypeMap
  , spanToTuple
  ) where

import Data.Bifunctor
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
preds_tis_ctx = [tis_var_ctx, tis_fun_ctx, tis_app_ctx, tis_lit_int_ctx, tis_lit_float_ctx, tis_lit_bool_ctx]

eisToTis :: (ExprSpan -> Double) -> ES -> Double
eisToTis _ (St _) = 0
eisToTis f (Ex e) = f e

eis_var :: ExprSpan -> Double
eis_var e = case e of
  Var {} -> 1
  _ -> 0

eis_fun :: ExprSpan -> Double
eis_fun e = case e of
  Lambda {} -> 1
  _ -> 0

eis_app :: ExprSpan -> Double
eis_app e = case e of
  Call {} -> 1
  _ -> 0

eis_lit_int :: ExprSpan -> Double
eis_lit_int e = case e of
  Int {} -> 1
  _ -> 0
eis_lit_float :: ExprSpan -> Double
eis_lit_float e = case e of
  Float {} -> 1
  _ -> 0
eis_lit_bool :: ExprSpan -> Double
eis_lit_bool e = case e of
  Bool {} -> 1
  _ -> 0

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

tis_var_ctx :: Feature
tis_var_ctx = ( mkContextLabels "Is-Var", mkContextFeatures (eisToTis eis_var) )

tis_fun_ctx :: Feature
tis_fun_ctx = ( mkContextLabels "Is-Fun", mkContextFeatures (eisToTis eis_fun) )

tis_app_ctx :: Feature
tis_app_ctx = ( mkContextLabels "Is-App", mkContextFeatures (eisToTis eis_app) )

tis_lit_int_ctx :: Feature
tis_lit_int_ctx = ( mkContextLabels "Is-Lit-Int", mkContextFeatures (eisToTis eis_lit_int) )
tis_lit_float_ctx :: Feature
tis_lit_float_ctx = ( mkContextLabels "Is-Lit-Float", mkContextFeatures (eisToTis eis_lit_float) )
tis_lit_bool_ctx :: Feature
tis_lit_bool_ctx = ( mkContextLabels "Is-Lit-Bool", mkContextFeatures (eisToTis eis_lit_bool) )
