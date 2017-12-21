module NanoML.ExampleFeatures
  ( preds_tis
  , preds_tis_ctx
  , only_ctx
  , Feature
  ) where

import Data.Bifunctor

import NanoML.Classify

import Language.Python.Common

import Debug.Trace

type Feature = ([String], (ES -> ES -> [Double]))

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
